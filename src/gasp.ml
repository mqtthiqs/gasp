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
  let init filename =
    let lexbuf = Lexing.from_function (Util.read_in_buffer b) in 
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
    lexbuf
  in
  SyntacticAnalysis.process
    ~lexer_init: init
    ~lexer_fun: SLF_lexer.main
    ~parser_fun: SLF_parser.signature
    ~input: filename  

let down = SLF_LF.sign $ LF_XLF.sign $ XLF_XLFe.sign $ XLFe_NLF.sign NLF.NLFSign.empty
let up = XLFe_NLF.from_sign $ XLF_XLFe.from_sign $ LF_XLF.from_sign $ SLF_LF.from_sign

let b = Util.buffer_of_file (List.hd filenames);;
let s = parse_buffer b (List.hd filenames);;
SLF_pp.sign Format.std_formatter s;;
print_string "=================================\n";;
let s' = (down $ up) s;;
Buffer.reset b;;
SLF_pp.sign (Format.formatter_of_buffer b) s';;
let s' = parse_buffer b (List.hd filenames);;
let s' = (down $ up) s';;
SLF_pp.sign Format.std_formatter s';;
if SLF.equals_sign SLF.Idmap.empty s s' then exit 0 else exit 1
