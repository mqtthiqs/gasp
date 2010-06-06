%{
  open AST

  let parse_error = Error.error "during parsing"
  let noname = 
    let c = ref 0 in
    fun () -> incr c; "*" ^ string_of_int !c
%}

%token EOF
%token LPAREN RPAREN COLON EQUAL TYPE ARROW DOT
%token<string> ID

%right DOT
%right ARROW
%start<AST.patch> patch

%%

patch: t=loc(term) EOF
{
  Patch t
}

term: s=loc(sort)
{
  Sort s
}
| LPAREN xs=ID+ COLON t=loc(term) RPAREN DOT s=loc(term)
{
  Position.value 
    (List.fold_left 
       (fun accu x -> 
	  Position.with_pos (Position.position accu) (Prod (x, t, accu))) 
       s (List.rev xs))
}
| LPAREN x=ID EQUAL a=loc(term) RPAREN DOT s=loc(term)
{
  SProd (x, a, s)
}
| t1=loc(term) ARROW t2=loc(term)
{
  Prod (noname (), t1, t2)
}
| x=loc(ID)
{
  Var x
}
| t=loc(term) x=loc(ID)
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
