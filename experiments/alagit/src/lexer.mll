{
 open Parser
 open Lexing
 open Error

 (** Keywords *)
 let keywords = [
   "Type", TYPE
 ]

 let is_keyword =
   let h = Hashtbl.create 13 in
     List.iter (fun (s, tok) -> Hashtbl.add h s tok) keywords;
     Hashtbl.find h

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

 (** Raise a lexical error message and stops the program. *)
 let lexical_error lexbuf msg =
   let pos = Position.lex_join lexbuf.lex_curr_p lexbuf.lex_curr_p in
   error "during lexical analysis" pos
     ("Unexpected token: `" ^ lexeme lexbuf ^ "'" ^ msg)

}

(*-------------------*
 | Lexical classes.  |
 *-------------------*)

(** Layout. *)

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

(** Alphanumeric characters classes. *)

let lowercase = ['a'-'z']

let uppercase = ['A'-'Z']

let identchar = ['A'-'Z' 'a'-'z' '0'-'9' '_']

let integer   = [ '0'-'9' ]

(** Identifiers. *)

let identifier  = identchar+

(*------------------*
 | Analysis rules.  |
 *------------------*)

rule main = parse

(** Layout. *)

| newline                               { next_line_and main lexbuf }
| blank+                                { main lexbuf }
| eof                                   { EOF }

(** Punctuation. *)
| "."                                   { DOT }
| ":"                                   { COLON }
| '('                                   { LPAREN }
| ')'                                   { RPAREN }

(** Symbols. *)
| '='                                   { EQUAL }
| "->"                                  { ARROW  }
| identifier                            { let id = lexeme lexbuf in
                                            try is_keyword id 
					    with Not_found -> ID (id)
                                        }

(** Comments *)

| "(*"                                  { comment 0 lexbuf }

(** Lexical error handling. *)

| _                                     
{ lexical_error lexbuf "." }

(*---------------------------*
 | Comments analysis rules.  |
 *---------------------------*)

and comment level = parse

(** Decrease the nesting level. *)

| "*)"                                  {
    if level = 0 then main lexbuf else comment (level - 1) lexbuf
}

(** Increase the nesting level. *)

| "(*"                                  {
    comment (level + 1) lexbuf
}

(** A comment must be closed before the end of the file. *)

| eof                                   {
    lexical_error lexbuf "Unterminated comment"
}

(** Layout. *)

| newline                               {
    next_line_and (comment level) lexbuf
}

(** Everything else is absorbed by the comment. *)

| _                                     {
    comment level lexbuf
}


