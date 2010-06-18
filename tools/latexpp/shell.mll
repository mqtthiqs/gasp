
(* shell preprocessor *)

{
  open Lexing
  open Format
  open Options

  let ocaml_keywords =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [
        "for"; "do"; "end"; "done"; "foreach"; "in";
        "while"; "if"; "then"; "fi"; "else"; "elif";
      ];       h

  let is_keyword = Hashtbl.mem ocaml_keywords

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
  | '$' { fprintf fmt "\\${}"; pp fmt lexbuf }
  | '~' { fprintf fmt "\\ensuremath{\\tilde{}}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  | ':'  { fprintf fmt "\\ensuremath{\\colon\\hspace{-0.1cm}}"; pp fmt lexbuf }
  | "::" { fprintf fmt "\\ensuremath{\\colon\\hspace{-0.1cm}
                                     \\colon\\hspace{-0.05cm}}";
           pp fmt lexbuf }
  | "++"  { fprintf fmt "{\\scriptsize\\ensuremath{++}}"; pp fmt lexbuf }
  | "<<"  { fprintf fmt "{\\scriptsize\\ensuremath{\\ll}}"; pp fmt lexbuf }
  | ">>"  { fprintf fmt "{\\scriptsize\\ensuremath{\\gg}}"; pp fmt lexbuf }
  | "=="  { fprintf fmt "{\\scriptsize\\ensuremath{==}}"; pp fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\ensuremath{\\backslash}"; pp fmt lexbuf }
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | ">=" { fprintf fmt "\\ensuremath{\\ge}"; pp fmt lexbuf }
  | "<=" { fprintf fmt "\\ensuremath{\\le}"; pp fmt lexbuf }
  | "&&" { fprintf fmt "\\ensuremath{\\land}"; pp fmt lexbuf }
  | "|" { fprintf fmt "\\ensuremath{|}"; pp fmt lexbuf }
  | "||" { fprintf fmt "\\ensuremath{\\lor}"; pp fmt lexbuf }
  | "[[" ([^'\n']* as s) "]]"
      {
	pp_print_string fmt s;
	pp fmt lexbuf
      }

  | "/*" [^'\n']* "*/" as s
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
  let shell_alltt fmt s =
    fprintf fmt "\\begin{alltt}";
    pp fmt (from_string s);
    fprintf fmt "\\end{alltt}%%\n"

  let shell_sf fmt s =
    fprintf fmt "\\bgroup\\sf\\medskip\\begin{flushleft}\n";
    start_of_line fmt (from_string s);
    fprintf fmt "\\end{flushleft}\\medskip\\egroup\\noindent\n"

  let () = Pp.add_pp_environment "shell-tt" shell_alltt
  let () = Pp.add_pp_environment "shell-sf" shell_sf
  let () = Pp.add_pp_environment "shell" shell_sf

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "shell-tt" texttt
  let () = Pp.add_pp_macro "shell-sf" textsf
  let () = Pp.add_pp_macro "shell" textsf
}
