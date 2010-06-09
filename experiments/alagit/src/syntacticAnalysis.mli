val process :
  lexer_init : ('a -> 'lexbuf) ->
  lexer_fun   : ('lexbuf -> 'token) ->
  parser_fun  : (('lexbuf -> 'token) -> 'lexbuf -> 'ast) ->
  input       : 'a ->
  'ast

val parse_file :
  string ->
  ((Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'ast) ->
  (Lexing.lexbuf -> 'token) -> 
  'ast
  
val parse_string :
  string ->
  ((Lexing.lexbuf -> 'token) -> Lexing.lexbuf -> 'ast) ->
  (Lexing.lexbuf -> 'token) -> 
  'ast
  
