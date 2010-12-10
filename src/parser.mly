%{
  open LF_AST

  let parse_error = Error.error "during parsing"

%}

%token EOF
%token COLON TYPE DOT LARROW RARROW 
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token<string> ID

%right RARROW
%left  LARROW
%left apply

%start<LF_AST.signature> program

%%

program: 
| t=signature { t }

signature:
| x=ID COLON t=loc(term) DOT s=signature { (x,t) :: s }
| EOF { [] }
| error { parse_error (Position.lex_join $startpos $endpos) "Syntax error." }

term: 
| LPAREN t=term RPAREN {t}
| x=ID { Var x }
| TYPE { Type }
| t=loc(term) RARROW u=loc(term) { Arr(t,u) }
| t=loc(term) LARROW u=loc(term) { Arr(u,t) }
| LBRACE x=loc(ID) COLON t=loc(term) RBRACE u=loc(term) { Prod(x, t, u) }
| LBRACKET x=loc(ID) COLON t=loc(term) RBRACKET u=loc(term) { Lam(x, t, u) }
| t=loc(term) u=loc(term) %prec apply { App (t, u) }


%inline loc(X): t=X 
{
  let pos = Position.lex_join $startpos $endpos in
  Position.with_pos pos t
}
