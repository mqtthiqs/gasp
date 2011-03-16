open AST
open Pprint

(* FIXME: Some parentheses are missing. *)

let rec program ds = sepmap break0 declaration ds ^^ break0

and declaration = function
  | DVal (b, e) -> 
    kw "val " ^^ binding b ^^ sym " = " ^^ group2 (break0 ^^ expression e)
  | DType (t, ds) ->
    kw "type " ^^ type_identifier t 
    ^^ sym " = " 
    ^^ group2 (break0
	       ^^ sepmap (break1 ^^ sym "+ ") constructor_declaration ds)
      
and expression = function
  | EVar x ->
    identifier x
  | EInt x ->
    sym (string_of_int x)
  | EUnit ->
    sym "()"
  | ECApp (k, []) ->
    constructor_identifier k
  | ECApp (k, es) ->
    constructor_identifier k ^^ space
    ^^ parens (sepmap (sym "," ^^ space) expression es)
  | (ESeq es) as from ->
    sepmap (sym ";" ^^ break1) (expression' from) es
  | (EVal (b, lhs, rhs)) as from ->
    kw "val " ^^ binding b 
    ^^ sym " = " ^^ group2 (expression lhs)
    ^^ break1 ^^ kw "in" ^^ break1 ^^ group (expression' from rhs)
  | EApp (EPrim1 Neg, e) ->
    sym "-" ^^ expression e
  | EApp (EApp (EPrim2 op, lhs), rhs) as from ->
    group2 (expression' from lhs) 
    ^^ group2 (break1 
	       ^^ sym (binop op)
	       ^^ break1 ^^ expression' from rhs)
  | EMatch (s, bs) ->
    kw "match" ^^ group (break1 ^^ expression s ^^ break1) 
    ^^ group2 (kw "with" ^^ break1
	       ^^ sepmap (break1 ^^ sym "|" ^^ space) branch bs)
    ^^ break1 ^^ kw "done"
  | EApp (lhs, rhs) ->
    expression lhs ^^ break1 ^^ expression rhs
  | EFun ((b, rty, body)) ->
    kw "fun" ^^ space ^^ parens (binding b) 
    ^^ space ^^ sym ":" ^^ space ^^ typ rty 
    ^^ space ^^ sym "=>" ^^ space 
    ^^ group2 (expression body)

  | EPrim1 _ | EPrim2 _ -> assert false

and expression' from e = 
  decide_paren (from, e) (expression e)
 
and decide_paren = function
  | (ESeq _, ESeq _) 
  | (ESeq _, EVal _) 
  | (EVal _, ESeq _) 
  | (EApp (EApp (EPrim2 Pow, _), _), 
     EApp (EApp (EPrim2 _, _), _))
  | (EApp (EApp (EPrim2 (Mul | Div | Mod), _), _), 
     EApp (EApp (EPrim2 (Add | Sub), _), _))
    -> parens
  | _ 
    -> identity

and branch (p, e) =
  group2 (pattern p ^^ space 
	  ^^ sym "->" ^^ break1 ^^ group2 (expression e))

and pattern = function
  | PVar x -> 
    identifier x
  | PCon (k, []) -> 
    constructor_identifier k
  | PCon (k, ps) -> 
    constructor_identifier k ^^ parens (sepmap (sym "," ^^ space) pattern ps)

and binop = function
  | Add  -> "+"
  | Mul  -> "*"
  | Pow  -> "**"
  | Div  -> "/"
  | Mod  -> "%"
  | Sub  -> "-"
  | Lt   -> "<"
  | Le   -> "<="
  | Gt   -> ">"
  | Ge   -> ">="
  | Eq   -> "="
  | Neq  -> "<>"

and constructor_declaration (k, tys) =
  constructor_identifier k 
  ^^ (if tys = [] then empty else parens (sepmap (sym "," ^^ break1) typ tys))

and function_definition (bs, rty, e) = 
  group2 (bindings bs ^^ space ^^ sym ":" ^^ typ rty ^^ space ^^ sym "=" 
	  ^^ break1 ^^ group2 (expression e))

and bindings bs = 
  parens (sepmap (sym "," ^^ break1) binding bs)

and typ = function
  | TInt ->
    kw "int"
  | TUnit ->
    kw "unit"
  | TVar t ->
    type_identifier t
  | TArrow (in_, out) ->
    parens (typ in_) 
    ^^ space ^^ sym "->" ^^ space
    ^^ typ out

and binding (x, t) = identifier x ^^ sym " : " ^^ typ t

and identifier (Identifier x) = text x

and constructor_identifier (CIdentifier x) = text x

and type_identifier (TIdentifier x) = text x

and sym x = text x

and kw x  = text x

and identity x = x


