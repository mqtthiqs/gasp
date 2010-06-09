%{
  open StlcdecAST

  let parse_error = Error.error "during parsing"

  let noname = 
    let c = ref 0 in
    fun () -> incr c; "_" ^ string_of_int !c
%}

%token EOF
%token LPAREN RPAREN COLON EQUAL TYPE ARROW LET FUN IN 
%token VAL 
%token<string> ID

%right ARROW
%start<StlcdecAST.fragment> fragment

%%

fragment: g=binding* d=declaration*  EOF
{
  Fragment (Env g, d)
}
| error
{
  parse_error (Position.lex_join $startpos $endpos) "Syntax error."
}

binding: VAL x=ID COLON ty=ty
{
  BindVar (x, ty)
}
| TYPE x=ID
{
  BindTyVar x
}

declaration: LET x=ID COLON ty=ty EQUAL e=expression
{
  DValue (x, ty, e)
}
| LET TYPE x=ID
{
  DType x
}

ty: x=ID
{
  TyVar x
}
| lhs=ty ARROW rhs=ty
{
  TyArrow (lhs, rhs)
}

expression: FUN x=ID COLON ty=ty ARROW e=expression1
{
  Lam (x, ty, e)
}
| LET x=ID COLON ty=ty EQUAL lhs=expression IN rhs=expression
{
  App (Lam (x, ty, rhs), lhs)
}
| e=expression1
{
  e
}

expression1: lhs=expression1 rhs=expression2 
{
  App (lhs, rhs)
}
| e=expression2
{
  e
}

expression2: x=ID
{
  Var x
}
| LPAREN e=expression RPAREN
{
  e
}

%inline loc(X): t=X 
{
  let pos = Position.lex_join $startpos $endpos in
  Position.with_pos pos t
}
