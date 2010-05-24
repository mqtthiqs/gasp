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
    Parser.patch lexer lexbuf
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

let parse_arith filename = 
  let parser lexer lexbuf = try
    Aparse.expr lexer lexbuf
  with
  | Parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> 
		    let lexbuf = Lexing.from_channel (open_in filename) in 
		    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
		    lexbuf
		    )
    ~lexer_fun: Alex.main
    ~parser_fun: (fun a b -> parser a b)
    ~input: filename

let process_arith filename =
  let e = parse_arith filename in
  print_string"done\n";flush_all();
  Print.arith Format.std_formatter e

let process filename = 
  let (AST.Patch t) = parse_file filename in
  let s = Check.infer_type Env.empty t in
  Print.ptype Format.std_formatter (AST.Sort s);
  Format.pp_print_newline Format.std_formatter()

let extension_of s = 
  try 
    let i = String.rindex s '.' in
    String.sub s (i+1) (String.length s-i-1)
  with Not_found -> ""

let _ =
  List.map (fun n -> 
	      match extension_of n with
		| "ga" -> process n 
		| "ar" -> process_arith n
		| _ -> failwith ("Bad extension "^n)) filenames
