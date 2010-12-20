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
    SLF_parser.signature lexer lexbuf
  with
    | SLF_parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> 
		    let lexbuf = Lexing.from_channel (open_in filename) in 
		    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
		    lexbuf
		 )
    ~lexer_fun: SLF_lexer.main
    ~parser_fun: parser
    ~input: filename

let sig_prec x = 42

let s = SLF_LF.sign_to_sign (parse_file (List.hd filenames))
let s = LF_XLF.sign [] s
let s' = XLF_XLFe.sign (List.rev s) 	(* TODO rev *)
let _ = XLFe_NLF.sign XLFe_NLF.CMap.empty NLF.NLFSign.empty s'
  
let _ =  
  XLF_pp.sign Format.std_formatter s
