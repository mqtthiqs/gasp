open Lexing
open Error

(** This function increments the line number in the buffer [lexbuf]
     and calls [f] on it. *)
let next_line_and f lexbuf  =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with
	  pos_lnum = pos.pos_lnum + 1;
	  pos_bol  = pos.pos_cnum;
      };
    f lexbuf

(** Buffer for string lexing. *)
let (add_to_string_buffer, get_string_buffer, reset_string_buffer) =
  let b = Buffer.create 13 in
    (Buffer.add_string b,
     (fun () -> Buffer.contents b),
     (fun () -> Buffer.clear b))

(** Hack from Stefano Zacchiroli. *)
let unescape_gen =
  let lex = lazy (Genlex.make_lexer []) in
    fun s ->
      let tok = Lazy.force lex (Stream.of_string s) in
        Stream.peek tok

let unescape_string s =
  match unescape_gen ("\"" ^ s ^ "\"") with
    | Some (Genlex.String s) -> s
    | _ -> assert false

let unescape_char s =
  match unescape_gen ("'" ^ s ^ "'") with
    | Some (Genlex.Char s) -> s
    | _ -> assert false

(** Raise a lexical error message and stops the program. *)
 let lexical_error lexbuf msg =
   let pos = Position.lex_join lexbuf.lex_curr_p lexbuf.lex_curr_p in
     error "during lexical analysis" pos
       ("Unexpected token: `" ^ lexeme lexbuf ^ "'" ^ msg)
