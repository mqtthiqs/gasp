let pretty_print = ref false
let parse_only   = ref false

let options = Arg.align [
  "-p", Arg.Set pretty_print, " Display the parsed input on stdout.";
  "--parse-only", Arg.Set parse_only, " Do syntax analysis only."
]

let parse_file filename =
  let parse lexer lexbuf = try
    Parser.program lexer lexbuf
  with
  | Parser.Error -> 
    Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> Lexing.from_channel (open_in filename))
    ~lexer_fun: Lexer.main
    ~parser_fun: parse
    ~input: filename

let filenames = ref []

let usage = "usage: stlc [options] input1 ... inputN"

let _ = Arg.parse options (fun s -> filenames := s :: !filenames) usage

let process_file filename = 
  let ast = parse_file filename in 
  if !pretty_print then 
    Pprint.Channel.pretty 0.8 80 stdout (Printer.program ast);
  ast
  
let asts = List.map process_file !filenames
  
let _ = 
  let signature = Parsers.parse_sign "spl-type-system.elf" in
  let repo = Repo.init signature in
  match asts with
    | [ast] ->
      let slf_tc_ast = TypeCheck.program ast in
      Format.printf "%a\n" SLF.Pp.term slf_tc_ast;
      Repo.check (Repo.commit repo slf_tc_ast)
    | _ -> 
      Error.global_error "Command line analysis" "Provide at most 2 files, please."
