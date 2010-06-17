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

meta(X): o=X 
{
  MetaAST.Object o
}

metalist(X): x=meta(X) xs=meta(metalist(X))
{
  MetaAST.Cons (x, xs)
}
| /* empty */
{
  MetaAST.Nil
}

fragment: g=meta(typing_environment) d=meta(declarations)  EOF
{
  Fragment (g, d)
}
| error
{
  parse_error (Position.lex_join $startpos $endpos) "Syntax error."
}

typing_environment: bs=meta(bindings)
{
  Env bs
}

bindings: bs=metalist(binding)
{
  MetaInternalize.on_list NoBinding 
  (fun x xs -> ConsBinding (x, xs)) bs
}

declarations: ds=metalist(declaration)
{
  MetaInternalize.on_list EmptyDeclarations 
  (fun x xs -> ConsDeclaration (x, xs)) ds
}

binding: VAL x=meta(ID) COLON ty=meta(ty)
{
  BindVar (x, ty)
}
| TYPE x=meta(ID)
{
  BindTyVar x
}

declaration: LET x=meta(ID) COLON ty=meta(ty) EQUAL e=meta(expression)
{
  DValue (x, ty, e)
}
| LET TYPE x=meta(ID)
{
  DType x
}

ty: x=meta(ID)
{
  TyVar x
}
| lhs=meta(ty) ARROW rhs=meta(ty)
{
  TyArrow (lhs, rhs)
}

expression: FUN LPAREN x=meta(ID) COLON ty=meta(ty) RPAREN ARROW e=meta(expression)
{
  Lam (x, ty, e)
}
| LET x=meta(ID) COLON ty=meta(ty) EQUAL lhs=meta(expression) IN rhs=meta(expression)
{
  App (MetaAST.Object (Lam (x, ty, rhs)), lhs)
}
| e=expression1
{
  e
}

expression1: lhs=meta(expression1) rhs=meta(expression2)
{
  App (lhs, rhs)
}
| e=expression2
{
  e
}

expression2: x=meta(ID)
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
