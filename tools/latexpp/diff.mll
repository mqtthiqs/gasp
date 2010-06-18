
(* Diff preprocessor *)

{
  open Lexing
  open Format
  open Options

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

rule pp fmt = parse
  | ('<'+ as cmd) ([^'\n']* as s)
      {
        fprintf fmt "{\\color{blue}$%s$" cmd;
	pp_print_string fmt s;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | ('|'+ as cmd) ([^'\n']* as s)
      {
        let b = Buffer.create 13 in
          String.iter (fun c -> 
                         Buffer.add_string b (Printf.sprintf "%c\\;\\;" c)) cmd;
        fprintf fmt "{\\color{green}$%s$" (Buffer.contents b);
	pp_print_string fmt s;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | ('>'+ as cmd) ([^'\n']* as s)
      {
        fprintf fmt "{\\color{red}$%s$" cmd;
	pp_print_string fmt s;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | ('='+ as cmd) ([^'\n']* as s)
      {
        fprintf fmt "{\\color{yellow}$%s$" cmd;
	pp_print_string fmt s;
	fprintf fmt "}";
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
  let diff_alltt fmt s =
    fprintf fmt "\\begin{alltt}";
    pp fmt (from_string s);
    fprintf fmt "\\end{alltt}%%\n"

  let diff_sf fmt s =
    fprintf fmt "\\bgroup\\sf\\medskip\\begin{flushleft}\n";
    start_of_line fmt (from_string s);
    fprintf fmt "\\end{flushleft}\\medskip\\egroup\\noindent\n"

  let () = Pp.add_pp_environment "diff-tt" diff_alltt
  let () = Pp.add_pp_environment "diff-sf" diff_sf
  let () = Pp.add_pp_environment "diff" diff_sf

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "diff-tt" texttt
  let () = Pp.add_pp_macro "diff-sf" textsf
  let () = Pp.add_pp_macro "diff" textsf
}
