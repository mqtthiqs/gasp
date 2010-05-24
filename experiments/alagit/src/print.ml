open AST
open Format

let ( ! ) = Position.value

let rec term fmt = function
  | Var x -> fprintf fmt "@[%s@]" x
  | App (t, x) -> fprintf fmt "@[%a @,@[%s@]@]" term' t x

and term' fmt t = term fmt !t

let rec sort fmt = function
  | KType -> fprintf fmt "Type"
  | KKind -> fprintf fmt "Kind"

let rec ptype fmt = function
  | Term t -> 
      fprintf fmt "@[%a@]" term' t
  | Sort s ->
      fprintf fmt "@[%a@]" sort s
  | Prod (x, t, s) -> 
      fprintf fmt "@[@[(%s : %a).@]@,@[%a@]@]" x ptype' t ptype' s
  | SProd (x, t, a, s) -> 
      fprintf fmt "@[@[(%s = %a : %a).@]@,%a@]" x term' a ptype' t ptype' s

and ptype' fmt t = ptype fmt !t

let term fmt t = fprintf fmt "@[%a@]" term t

let ptype fmt t = fprintf fmt "@[%a@]" ptype t

(* let subst fmt s = *)
(*   Subst.fold (fun n k () -> fprintf fmt "@[(%s -> %d)@]" n (Obj.magic k)) s (); *)

open Arith

let rec arith fmt = function
  | O -> fprintf fmt "O"
  | S a -> fprintf fmt "@[(S %a)@]" arith a
  | And (a,b) -> fprintf fmt "@[(%a and %a)@]" arith a arith b
  | Or (a,b) -> fprintf fmt "@[(%a or %a)@]" arith a arith b
  | Impl (a,b) -> fprintf fmt "@[(%a -> %a)@]" arith a arith b
  | Plus (a,b) -> fprintf fmt "@[(%a + %a)@]" arith a arith b
  | Minus (a,b) -> fprintf fmt "@[(%a - %a)@]" arith a arith b
  | Eq (a,b) -> fprintf fmt "@[(%a = %a)@]" arith a arith b
  | Neq (a,b) -> fprintf fmt "@[(%a /= %a)@]" arith a arith b
  | Le (a,b) -> fprintf fmt "@[(%a <= %a)@]" arith a arith b
