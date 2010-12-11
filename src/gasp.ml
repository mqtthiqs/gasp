open Pp

let options = Arg.align
  [
    "--debug", Arg.Set Settings.debug, 
    " Set debug mode.";
  ]

let usage_msg =
  Printf.sprintf "%s:" (Filename.basename Sys.executable_name)

let filenames =
  let filenames = ref [] in
    Arg.parse options (fun f -> filenames := f :: !filenames) usage_msg;
    !filenames

let parse_file filename =
  let parser lexer lexbuf = try
    Parser.signature lexer lexbuf
  with
    | Parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> 
		    let lexbuf = Lexing.from_channel (open_in filename) in 
		    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
		    lexbuf
		 )
    ~lexer_fun: Lexer.main
    ~parser_fun: parser
    ~input: filename

let sig_prec x = 42

let _ =
  LF_pp.signature Format.std_formatter
    (LF_pretype.sign_of_ast
       (parse_file (List.hd filenames)))
