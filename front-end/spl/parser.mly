%{
  (** This module implements syntactic analysis. *)

  open AST

  let parse_error = Error.error "during parsing"

  (* [fresh_identifier ()] produces an identifier that 
     has never been used before. *)
  let fresh_identifier = 
    let c = ref 0 in
    fun () -> incr c; Identifier ("__" ^ string_of_int !c)

%}

(* Keywords *)
%token MATCH DONE WITH VAL IN TYPE FUN INT UNIT

(* Symbols *)
%token COMMA SEMICOLON COLON LPAREN RPAREN PIPE RARROW DRARROW PLUS MINUS STAR STARSTAR SLASH
%token EQ NEQ GT LT LE GE PERCENT UNDERSCORE LBRACKET RBRACKET
(* Literals *)
%token<int>    LINT

(* Identifiers *)
%token<string> UID ID

%token EOF

%start<AST.program> program

%right DRARROW
%right IN 
%right SEMICOLON

%nonassoc EQ NEQ GT GE LE LT
%left PLUS MINUS 
%left STAR SLASH PERCENT
%right STARSTAR

%right RARROW

%%

program: 
ds=declaration+ EOF
{
  ds
}

declaration:
VAL b=binding EQ def=expression0
{
  DVal (b, def)
}
| TYPE t=type_identifier EQ ks=separated_list(PLUS, constructor_declaration)
{
  DType (t, ks)
}
(* Future syntactic sugar: 
| FUN f=identifier 
  LPAREN bs=separated_list(COMMA, binding) RPAREN COLON rty=typ EQ e=expression
{
  DFun (f, (bs, rty, e))
}
*)

binding:
x=identifier COLON ty=typ 
{
  (x, ty)
}

expression0:
VAL b=binding EQ lhs=expression0 IN rhs=expression0
{
  EVal (b, lhs, rhs)
}
| MATCH s=expression0 WITH PIPE? bs=separated_list(PIPE, branch) DONE
{
  EMatch (s, bs)
}
| FUN LPAREN b=binding RPAREN COLON rty=typ DRARROW body=expression0
{
  EFun ((b, rty, body))
}
| lhs=expression0 SEMICOLON rhs=expression0
{
  match rhs with
    | ESeq es -> ESeq (lhs :: es)
    | _ -> ESeq [ lhs; rhs ]
}
| e=expression1
{
  e
}

expression1:
lhs=expression1 b=binop rhs=expression1
{
  EApp (EApp (EPrim2 b, lhs), rhs)
}
| MINUS e=expression2 
{
  EApp (EPrim1 Neg, e)
}
| e=expression2 
{
  e
}

expression2:
| lhs=expression3 rhs=expression2
{
  EApp (lhs, rhs)
}
| e=expression3
{
  e
}

expression3:
x=identifier
{
  EVar x
}
| i=LINT
{
  EInt i
}
| LPAREN RPAREN
{
  EUnit
}
| k=constructor_identifier 
{
  ECApp (k, [])
}
| k=constructor_identifier LBRACKET args=separated_list(COMMA, expression1) RBRACKET
{
  ECApp (k, args)
}
| LPAREN e=expression0 RPAREN
{
  e
}


branch: p=pattern RARROW e=expression0
{
  (p, e)
}

pattern:
  x=identifier
{
  PVar x
}
| UNDERSCORE 
{
  PVar (fresh_identifier ())
}
| k=constructor_identifier
{
  PCon (k, [])
}
| k=constructor_identifier LPAREN ps=separated_list(COMMA, pattern) RPAREN
{
  PCon (k, ps)
}

%inline binop: 
  PLUS 
{
  Add
}
| STAR
{
  Mul
}
| SLASH
{
  Div
}
| MINUS
{
  Sub
}
| PERCENT
{
  Mod
}
| STARSTAR
{
  Pow
}
| LE
{
  Le
}
| GE
{
  Ge
}
| GT
{
  Gt
}
| LT
{
  Lt
}
| EQ
{
  Eq
}
| NEQ
{
  Neq
}

constructor_declaration: k=constructor_identifier LPAREN tys=separated_list(COMMA, typ) RPAREN
{
  (k, tys)
}
| k=constructor_identifier
{
  (k, [])
}

typ:
INT
{
  TInt
}
| UNIT 
{
  TUnit
}
| tv=type_identifier
{
  TVar tv
}
| lhs=typ RARROW rhs=typ
{
  TArrow (lhs, rhs)
}

identifier: x=ID
{
  Identifier x
}

type_identifier: x=ID
{
  TIdentifier x
}
  
constructor_identifier: k=UID
{
  CIdentifier k
}



