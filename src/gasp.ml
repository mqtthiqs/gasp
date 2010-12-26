open Pp
open Util

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
  
let parse_buffer b filename =
  let read =
    let off = ref 0 in
    fun s n ->
      let rest = min (Buffer.length b - !off) n in
      if rest <= 0 then 0 else
	(Buffer.blit b !off s 0 rest; off := !off + rest; rest)
  in
  let parser lexer lexbuf = try
    SLF_parser.signature lexer lexbuf
  with
    | SLF_parser.Error -> Error.error "Parsing" (Position.cpos lexbuf) "Unknown error.\n"
  in
  SyntacticAnalysis.process
    ~lexer_init: (fun filename -> 
		    let lexbuf = Lexing.from_function read in 
		    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
		    lexbuf
		 )
    ~lexer_fun: SLF_lexer.main
    ~parser_fun: parser
    ~input: filename  

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

let buffer_of_file filename =
  let b = Buffer.create 16 in
  let ic = open_in filename in
  let rec read () = 
    try Buffer.add_channel b ic 1; read()
    with End_of_file -> ()
  in read (); b

let down = SLF_LF.sign $ LF_XLF.sign $ XLF_XLFe.sign
let up = XLF_XLFe.from_sign $ LF_XLF.from_sign $ SLF_LF.from_sign

let b = buffer_of_file (List.hd filenames);;
let s = parse_buffer b (List.hd filenames);;
let s = (down $ up) s;;
Buffer.reset b;;
SLF_pp.sign (Format.formatter_of_buffer b) s;;
let s' = parse_buffer b (List.hd filenames);;
let s' = (down $ up) s';;
SLF_pp.sign Format.std_formatter s;;
print_string "=================================\n";
SLF_pp.sign Format.std_formatter s';;
(* if SLF.equals_sign s s' then exit 0 else exit 1 *)
