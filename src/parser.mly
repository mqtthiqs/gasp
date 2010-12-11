%{
  open LF_AST

  let parse_error = Error.error "during parsing"
%}

%token EOF
%token COLON TYPE DOT LARROW RARROW 
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token<string> ID

%nonassoc LBRACKET LBRACE
%right RARROW LARROW
%nonassoc LPAREN
%left APPLY TYPE ID

%start<LF_AST.signature> signature

%%

signature:
| ds=declaration* EOF { ds }
| error { parse_error (Position.lex_join $startpos $endpos) "Syntax error." }

declaration:
| x=ID COLON t=loc(term) DOT { (x,t) }

term: 
| LPAREN t=term RPAREN {t}
| x=ID { Var x }
| TYPE { Type }
| t=loc(term) RARROW u=loc(term) { Arr(t,u) }
| t=loc(term) LARROW u=loc(term) { Arr(u,t) }
| LBRACE xs=loc(ID)+ COLON t=loc(term) RBRACE u=loc(term) %prec LBRACE 
    { Position.value 
	(List.fold_left 
	   (fun acc x -> 
	      Position.with_pos Position.dummy (Prod(x, t, acc))) u (List.rev xs)) }
| LBRACKET xs=loc(ID)+ COLON t=loc(term) RBRACKET u=loc(term) %prec LBRACKET
    { Position.value 
	(List.fold_left 
	   (fun acc x -> 
	      Position.with_pos Position.dummy (Lam(x, t, acc))) u (List.rev xs)) }
| t=loc(term) u=loc(term) %prec APPLY { App (t, u) }

%inline loc(X): t=X 
{
  let pos = Position.lex_join $startpos $endpos in
  Position.with_pos pos t
}
