%{
  open AST

  let parse_error = Error.error "during parsing"
  let noname () = Name.fresh "*"
%}

%token EOF
%token LPAREN RPAREN COLON EQUAL TYPE ARROW DOT
%token<string> ID

%right DOT
%right ARROW
%start<AST.patch> patch

%%

patch: t=loc(ptype) EOF
{
  Patch t
}

ptype: a=loc(term)
{
  Term a
}
| s=sort
{
  Sort s
}
| LPAREN xs=id+ COLON t=loc(ptype) RPAREN DOT s=loc(ptype)
{
  Position.value 
    (List.fold_left 
       (fun accu x -> 
	  Position.with_pos (Position.position accu) (Prod (x, t, accu))) 
       s (List.rev xs))
}
| LPAREN x=id EQUAL a=loc(term) COLON t=loc(ptype) RPAREN DOT s=loc(ptype)
{
  SProd (x, t, a, s)
}
| t1=loc(ptype) ARROW t2=loc(ptype)
{
  Prod (noname (), t1, t2)
}

id: x=ID
{
  Name.from_string x
}

term: x=id
{
  Var x
}
| t=loc(term) x=id
{
  App (t, x)
}

sort: TYPE
{
  KType
}

%inline loc(X): t=X 
{
  let pos = Position.lex_join $startpos $endpos in
  Position.with_pos pos t
}
