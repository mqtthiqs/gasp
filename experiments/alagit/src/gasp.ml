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

let parse_file input parser_fun lexer_fun =
  let parser_fun lexer lexbuf = try
    parser_fun lexer lexbuf
  with
  | Parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process 
    ~lexer_init:ExtLexing.lexer_init ~lexer_fun ~parser_fun ~input

let process filename = 
  let (AST.Patch t) = parse_file filename Parser.patch Lexer.main in
  let s = Check.infer_type Env.empty t in
  Print.ptype Format.std_formatter (AST.Sort s);
  Format.pp_print_newline Format.std_formatter()

let process_stlcdec filename = 
  let _fragment_ast = 
    parse_file filename StlcdecParser.fragment StlcdecLexer.main
  in
  ()

let init_stcldec_repository () = 
  let _repository = StlcdecInternalize.initial_repository in 
  ()

let _ =
  List.map process filenames

