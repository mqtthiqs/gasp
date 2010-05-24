{
  open Arith
  open Aparse
  open Lexing
}

let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
let integer   = [ '0'-'9' ]

rule main = parse
| newline             { main lexbuf }
| blank+              { main lexbuf }
| eof                 { EOF }
| "and" {AND} 
| "or" {OR} 
| "->" {IMPL} 
| "+"  {PLUS} 
| "-"  {MINUS} 
| "="  {EQ} 
| "/=" {NEQ} 
| "<=" {LE} 
| "(" {LPAREN} 
| ")" {RPAREN}
| "O" {ZERO}
| "S" {SUCC}
| integer+ as num {INT (int_of_string num)}
| _                  {    Error.error "during lexical analysis" Position.dummy
			    ("Unexpected token: `" ^ lexeme lexbuf ^ "'") }
