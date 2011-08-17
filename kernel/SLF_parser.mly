%{
  open SLF
  open Definitions

  let parse_error = Error.error "during parsing"

  let fresh = 
    let x = ref 0 in 
    fun () -> incr x; "_" ^ string_of_int !x

  let mk_arrow u t = 
    Prod (fresh (), u, t)

%}

%token EOF
%token COLON TYPE DOT LARROW RARROW UNDERSCORE EQ
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE OPEN IN DEF
%token<string> ID

%start<SLF.sign> signature
%start<SLF.term> terml

%%

signature:
ds=declaration* EOF 
    { 
      List.flatten ds 
    }
| error 
{ 
  parse_error (Position.lex_join $startpos $endpos) "Syntax error." 
}

declaration:
  xs=ID+ COLON t=loc(term) DOT 
{ 
  List.map (fun x -> (x, t)) xs 
}

name:
  id=ID 
{ 
  id 
}
| UNDERSCORE 
{ 
  fresh ()
}

term1:
  t=loc(term2) RARROW u=loc(term1) 
{ 
  mk_arrow t u
}
| t=loc(term1) LARROW u=loc(term2) 
{ 
  mk_arrow u t
}
| LBRACE xs=ID+ COLON t=loc(term1) RBRACE u=loc(term1)
{ 
  Position.value 
  (List.fold_left 
     (fun acc x -> 
       Position.with_pos Position.dummy (Prod(x, t, acc))) u (List.rev xs)) 
}
| x=term2 
{ 
  x 
}

term2:
  t=loc(term2) u=loc(term3) 
{ 
  App (t, u) 
}
| x=term3 
{ 
  x 
}

term3: 
  LPAREN t=term1 RPAREN 
{
  t
}
| LBRACKET b=binding RBRACKET u=loc(term2)
{
  Lam (fst b, snd b, u)
}
| LBRACKET xs=paren(binding)+ RBRACKET u=loc(term2)
{ 
  Position.value 
  (List.fold_left 
     (fun acc (x, t) -> 
       Position.with_pos Position.dummy (Lam(x, t, acc))) u (List.rev xs)) 
}
| x=ID 
{ 
  Ident x 
}
| DEF x=ID EQ u=loc(term1) IN t=loc(term1)
{
  Def (Definitions.Define ((x --> u) None, t))
}
| OPEN x=ID IN t=loc(term1) 
{ 
  Def (Definitions.Open (x, t))
}
| TYPE 
{ 
  Type 
}

%inline term: x=term1 
{ 
  x 
}

terml : x=loc(term) EOF 
{
  x
}

binding: x=name COLON ty=loc(term) 
{
  (x, ty)
}

paren(X): LPAREN x=X RPAREN
{
  x
}

%inline loc(X): t=X 
{
  let pos = Position.lex_join $startpos $endpos in
  Position.with_pos pos t
}
