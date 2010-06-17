%{
  open StlcdecAST

  let parse_error = Error.error "during parsing"

  let noname = 
    let c = ref 0 in
    fun () -> incr c; "_" ^ string_of_int !c
%}

%token EOF
%token LPAREN RPAREN COLON EQUAL TYPE ARROW LET FUN IN 
%token VAL LABRACKET RABRACKET DOT IMPORT 
%token<string> ID

%right ARROW
/* %nonassoc empty_metalist
%nonassoc LABRACKET  */

%start<StlcdecAST.fragment> fragment

%%

/******************************************************************************/
/* Meta(X) concrete syntax grammar                                            */
/******************************************************************************/

%inline obj(X): o=X
{
  MetaAST.Object o
}

%inline meta(X): LABRACKET x=ID EQUAL o=X RABRACKET
{
  MetaAST.Define (Name.fresh x, MetaAST.Object o)
}
| LABRACKET x=ID RABRACKET
{
  MetaAST.Reference (Name.from_internal_string x)
}

/* 
   I wish Menhir accepted a definition of the form "meta(X): x=objx(X)". 
   But this creates a (purely syntactic)  cycle that is rejected by Menhir. 
   Damned! This forces us to generate a *lot* of specialized RHS in the sequel 
   of the grammar. 
   (This means that I have to extend Menhir to accept some well-founded
    recursive inline parameterized non terminal definitions.)
*/

metalist(S, X): x=meta(X) S xs=obj(metalist(S, X))
{
  MetaAST.Cons (x, xs)
}
| x=obj(X) S xs=obj(metalist(S, X)) 
{
  MetaAST.Cons (x, xs) 
}
| /* empty */ /* %prec empty_metalist */
{
  MetaAST.Nil
}

nemetalist(S, X): x=meta(X) S xs=obj(nemetalist(S, X))
{
  MetaAST.Cons (x, xs) 
}
| x=obj(X) S xs=obj(nemetalist(S, X)) 
{
  MetaAST.Cons (x, xs)
}
| x=meta(X) S
{
  MetaAST.Cons (x, MetaAST.Object MetaAST.Nil)
}
| x=obj(X) S
{
  MetaAST.Cons (x, MetaAST.Object MetaAST.Nil)
}

/******************************************************************************/
/* STLCdec concrete syntax grammar                                            */
/******************************************************************************/

/* We allow meta code that works on a group of declarations. */
fragment: g=obj(typing_environment) d=meta(declarations)  EOF
{
  Fragment (g, d)
}
| g=obj(typing_environment) d=obj(declarations) EOF
{
  Fragment (g, d)
}
| error
{
  parse_error (Position.lex_join $startpos $endpos) "Syntax error."
}

/* We allow meta code that works on a group of bindings. */
%inline typing_environment: IMPORT bs=meta(bindings) IN
{
  Env bs
}
| IMPORT bs=obj(bindings) IN
{
  Env bs
}
| /* empty */
{
  Env (MetaAST.Object NoBinding)
}

/* We allow meta code that works on a particular binding. */
bindings: bs=nemetalist(DOT, binding)
{
  MetaInternalize.on_list NoBinding 
  (fun x xs -> ConsBinding (x, xs)) bs
}


declarations: ds=metalist(DOT, declaration)
{
  MetaInternalize.on_list EmptyDeclarations 
  (fun x xs -> ConsDeclaration (x, xs)) ds
}

binding: VAL x=obj(ID) COLON ty=obj(ty)
{
  BindVar (x, ty)
}
/* We allow meta code that works on a identifier. */
| VAL x=meta(ID) COLON ty=obj(ty)
{
  BindVar (x, ty)
}
/* We allow meta code that works on a type. */
| VAL x=obj(ID) COLON ty=meta(ty)
{
  BindVar (x, ty)
}
/* We allow both simultaneously. */
| VAL x=meta(ID) COLON ty=meta(ty)
{
  BindVar (x, ty)
}
| TYPE x=obj(ID)
{
  BindTyVar x
}
/* We allow meta code that works on a type identifier 
   declaration. */
| TYPE x=meta(ID)
{
  BindTyVar x
}

declaration: LET x=obj(ID) COLON ty=obj(ty) EQUAL e=obj(expression)
{
  DValue (x, ty, e)
}
| LET x=meta(ID) COLON ty=obj(ty) EQUAL e=obj(expression)
{
  DValue (x, ty, e)
}						    
| LET x=obj(ID) COLON ty=meta(ty) EQUAL e=obj(expression)
{
  DValue (x, ty, e)
}						    
| LET x=obj(ID) COLON ty=meta(ty) EQUAL e=meta(expression)
{
  DValue (x, ty, e)
}						    
| LET TYPE x=obj(ID)
{
  DType x
}
| LET TYPE x=meta(ID)
{
  DType x
}

ty: x=obj(ID)
{
  TyVar x
}
| lhs=obj(ty) ARROW rhs=obj(ty)
{
  TyArrow (lhs, rhs)
}

expression: FUN LPAREN x=obj(ID) COLON ty=obj(ty) RPAREN ARROW e=obj(expression)
{
  Lam (x, ty, e)
}
| LET x=obj(ID) COLON ty=obj(ty) EQUAL lhs=obj(expression) IN rhs=obj(expression)
{
  App (MetaAST.Object (Lam (x, ty, rhs)), lhs)
}
| e=expression1
{
  e
}

expression1: lhs=obj(expression1) rhs=obj(expression2)
{
  App (lhs, rhs)
}
| e=expression2
{
  e
}

expression2: x=obj(ID)
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
