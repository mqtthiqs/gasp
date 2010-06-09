exception ParsingError

type 'token with_pos = 'token * Lexing.position * Lexing.position

let parsing_step = "during parsing"

let process ~lexer_init ~lexer_fun ~parser_fun ~input =
  parser_fun lexer_fun (lexer_init input)

let process ~lexer_init ~lexer_fun ~parser_fun ~input = try
  process ~lexer_init ~lexer_fun ~parser_fun ~input
with Sys_error msg ->
  Error.global_error parsing_step msg

let parse lexer_init input parser_fun lexer_fun =
  let parser_fun lexer lexbuf = try
    parser_fun lexer lexbuf
  with
  | Parser.Error -> 
      Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  process ~lexer_init ~lexer_fun ~parser_fun ~input

let parse_file input parser_fun lexer_fun = 
  parse ExtLexing.lexer_init input parser_fun lexer_fun

let parse_string input parser_fun lexer_fun = 
  parse Lexing.from_string input parser_fun lexer_fun

