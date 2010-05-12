val process :
  lexer_init : ('a -> 'lexbuf) ->
  lexer_fun  : ('lexbuf -> 'token) ->
  parser_fun : (('lexbuf -> 'token) -> 'lexbuf -> 'ast) ->
  input      : 'a ->
  'ast
