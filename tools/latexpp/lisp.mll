
(* Lisp preprocessor *)

{
  open Lexing
  open Format
  open Options

  let linenumber = ref false

  let lisp_keywords =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [
	"defun"; "if"; "let"; "set"; "setq"; "cond"; "car"; "cdr"; "caar"; 
	"cadar"; "null"; "cons"; "caddr"; "eq"; "atom"; "cadr"
      ];
    h

  let is_keyword = Hashtbl.mem lisp_keywords

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

  let lineno = ref 0

  let indentation fmt n =
    let space = 0.5 *. (float n) in
      if !linenumber then begin
        incr lineno;
        fprintf fmt "\n\\noindent %d \\hspace*{%2.2fem}" !lineno space
      end else
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
  | ";;" [^'\n']* as s
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
  let pp lnf fmt s =
    linenumber := lnf;
    pp fmt s

  let start_of_line lnf fmt s =
    linenumber := lnf;
    start_of_line fmt s

  let lisp_alltt linenumber fmt s =
    fprintf fmt "\\begin{alltt}";
    pp linenumber fmt (from_string s);
    fprintf fmt "\\end{alltt}%%\n"

  let lisp_sf linenumber fmt s =
    fprintf fmt "\\bgroup\\sf\\medskip\\begin{flushleft}\n";
    start_of_line linenumber fmt (from_string s);
    fprintf fmt "\\end{flushleft}\\medskip\\egroup\\noindent\n"

  let () = Pp.add_pp_environment "lisp-tt" (lisp_alltt false)
  let () = Pp.add_pp_environment "lisp-sf" (lisp_sf false)
  let () = Pp.add_pp_environment "lisp" (lisp_sf false)
  let () = Pp.add_pp_environment "lisp-tt-ln" (lisp_alltt true)
  let () = Pp.add_pp_environment "lisp-sf-ln" (lisp_sf true)
  let () = Pp.add_pp_environment "lisp-ln" (lisp_sf true)

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    pp false fmt (from_string s);
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    pp false fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "lisp-tt" texttt
  let () = Pp.add_pp_macro "lisp-sf" textsf
  let () = Pp.add_pp_macro "lisp" textsf
}
