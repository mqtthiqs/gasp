
(* ada preprocessor *)

{
  open Lexing
  open Format
  open Options

  let ada_keywords =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [
        "abort";  "else";  "new";  "return";
        "abs";  "elsif";  "not";  "reverse";
        "abstract";  "end";  "null";
        "accept";  "entry";   "select";
        "access";  "exception";  "of";  "separate";
        "aliased";  "exit";  "or";  "subtype";
        "all";   "others";  "synchronized";
        "and";  "for";  "out";
        "array";  "function";  "overriding";  "tagged";
        "at";    "task";
        "generic";  "package";  "terminate";
        "begin";  "goto";  "pragma";  "then";
        "body";   "private";  "type";
        "if";  "procedure";
        "case";  "in";  "protected";  "until";
        "constant";  "interface";   "use";
        "is";  "raise";
        "declare";   "range";  "when";
        "delay";  "limited";  "record";  "while";
        "delta";  "loop";  "rem";  "with";
        "digits";   "renames";
        "do";  "mod";  "requeue"; "xor";
      ];
    h

  let is_keyword = Hashtbl.mem ada_keywords

  let tab_size = 8

  let count_spaces s =
    let c = ref 0 in
    for i = 0 to String.length s - 1 do
      c := !c + (
        if s.[i] = '\t' then
	  tab_size - (!c mod tab_size)
	else
	  1
      )
    done;
    !c

  let indentation fmt n =
    let space = 0.5 *. (float n) in
    fprintf fmt "\n\\noindent\\hspace*{%2.2fem}" space

  let print_ident fmt =
    let char = function
      | '_' -> pp_print_string fmt "\\_{}"
      | c -> pp_print_char fmt c
    in
    String.iter char

  let color () = is_set "color"

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule pp fmt = parse
  | '{'  { fprintf fmt "\\{"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; pp fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  | '^'  { fprintf fmt "\\^{}"; pp fmt lexbuf }
  | ':'  { fprintf fmt "\\ensuremath{\\colon}"; pp fmt lexbuf }
  | "::"  { fprintf fmt "\\ensuremath{\\colon\\colon}"; pp fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\ensuremath{\\backslash}"; pp fmt lexbuf }
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "<-" { fprintf fmt "\\ensuremath{\\leftarrow}"; pp fmt lexbuf }
  | ">=" { fprintf fmt "\\ensuremath{\\ge}"; pp fmt lexbuf }
  | "<=" { fprintf fmt "\\ensuremath{\\le}"; pp fmt lexbuf }
  | "&&" { fprintf fmt "\\ensuremath{\\land}"; pp fmt lexbuf }
  | "|" { fprintf fmt "\\ensuremath{|}"; pp fmt lexbuf }
  | "||" { fprintf fmt "\\ensuremath{\\lor}"; pp fmt lexbuf }
  | "==" { fprintf fmt "\\ensuremath{\\equiv}"; pp fmt lexbuf }
  | "!=" { fprintf fmt "\\ensuremath{\\not\\equiv}"; pp fmt lexbuf }
  | "<>" { fprintf fmt "\\ensuremath{\\not=}"; pp fmt lexbuf }
  | "'a" { fprintf fmt "\\ensuremath{\\alpha}"; pp fmt lexbuf }
  | "*"  { fprintf fmt "\\ensuremath{\\times}"; pp fmt lexbuf }
  | "(*" [^'\n']* "*)" as s
      {
	fprintf fmt "\\emph{"; if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt s; fprintf fmt "}";
	pp fmt lexbuf
      }
  | ident as s
      {
	if is_keyword s then begin
	  if color () then fprintf fmt "{\\color{blue}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else
          print_ident fmt s;
	pp fmt lexbuf
      }
  | "\n"
      { fprintf fmt "~\\linebreak"; start_of_line fmt lexbuf }
  | "\n" space* eof
      { pp_print_string fmt "\n" }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; pp fmt lexbuf }

and start_of_line fmt = parse
  | space* as s
      { indentation fmt (count_spaces s); pp fmt lexbuf }
  | eof
      { () }

{
  let ada_alltt fmt s =
    fprintf fmt "\\begin{alltt}";
    pp fmt (from_string s);
    fprintf fmt "\\end{alltt}%%\n"

  let ada_sf fmt s =
    fprintf fmt "\\bgroup\\sf\\medskip\\begin{flushleft}\n";
    start_of_line fmt (from_string s);
    fprintf fmt "\\end{flushleft}\\medskip\\egroup\\noindent\n"

  let () = Pp.add_pp_environment "ada-tt" ada_alltt
  let () = Pp.add_pp_environment "ada-sf" ada_sf
  let () = Pp.add_pp_environment "ada" ada_sf

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "ada-tt" texttt
  let () = Pp.add_pp_macro "ada-sf" textsf
  let () = Pp.add_pp_macro "ada" textsf
}
