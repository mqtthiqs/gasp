%{
  open SLF

  let parse_error = Error.error "during parsing"
%}

%token EOF
%token COLON TYPE DOT LARROW RARROW DOLLAR EQUALS BIGRARROW
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token<string> ID

%start<SLF.sign> signature
%start<SLF.term> terml

%%

signature:
  ds=declaration* EOF { ds }
| error { parse_error (Position.lex_join $startpos $endpos) "Syntax error." }

declaration:
  x=ID COLON t=loc(term) DOT { (x, Decl t) }

subst:
  LPAREN x=ID EQUALS t=loc(term) RPAREN { x, t}

term1:
  t=loc(term2) RARROW u=loc(term1) { Arr(t,u) }
| t=loc(term1) LARROW u=loc(term2) { Arr(u,t) }
| LBRACE xs=ID+ COLON t=loc(term1) RBRACE u=loc(term1)
    { Position.value 
	(List.fold_left 
	   (fun acc x -> 
	      Position.with_pos Position.dummy (Prod(x, t, acc))) u (List.rev xs)) }
| x=term2 { x }

term2:
  t=loc(term2) u=loc(term3) { App (t, u) }
| x=term3 { x }

term3: 
  LPAREN t=term1 RPAREN {t}
| LBRACKET xs=ID+ COLON t=loc(term1) RBRACKET u=loc(term1)
    { Position.value 
	(List.fold_left 
	   (fun acc x -> 
	      Position.with_pos Position.dummy (Lam(x, t, acc))) u (List.rev xs)) }
| x=ID { Var x }
| LBRACE p=ID BIGRARROW t=loc(term1) RBRACE s=subst* { Box(t, p, s) }
| DOLLAR x=ID { Meta x }
| TYPE { Type }

term: x=term1 { x }

terml : x=loc(term) EOF {x}

%inline loc(X): t=X 
{
  let pos = Position.lex_join $startpos $endpos in
  Position.with_pos pos t
}
