{
  open SLF_parser
  open Lexing
  open ExtLexing
  open Error

  (** Keywords *)
  let keywords = [
    "type", TYPE
  ]

  let is_keyword =
    let h = Hashtbl.create 13 in
    List.iter (fun (s, tok) -> Hashtbl.add h s tok) keywords;
    Hashtbl.find h
}

(*-------------------*
 | Lexical classes.  |
 *-------------------*)

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '0'-'9' '_' '-' '\'' '*' '+' '=' '>' '<' ',' ';']
let integer   = [ '0'-'9' ]
let identifier  = identchar+

(*------------------*
 | Analysis rules.  |
 *------------------*)

rule main = parse
  | newline                               { next_line_and main lexbuf }
  | blank+                                { main lexbuf }
  | eof                                   { EOF }
  | "."                                   { DOT }
  | ":"                                   { COLON }
  | '('                                   { LPAREN }
  | ')'                                   { RPAREN }
  | '['                                   { LBRACKET }
  | ']'                                   { RBRACKET }
  | '{'                                   { LBRACE }
  | '}'                                   { RBRACE }
  | "->"                                  { RARROW  }
  | "<-"                                  { LARROW  }
  | "=>"                                  { BIGRARROW  }
  | "="                                   { EQUALS }
  | identifier                            { let id = lexeme lexbuf in
                                            try is_keyword id 
					    with Not_found -> ID (id)
                                          }
  | "%"                                  { comment lexbuf }
  | _                                     
      { lexical_error lexbuf "." }
      
and comment = parse
  | newline                               { next_line_and main lexbuf }
  | eof                                   { EOF }
  | _                                     { comment lexbuf }
