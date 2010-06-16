let patch_from_string ?(internal=false) s : AST.patch = 
  let module S = struct let internal = internal end in
  let module P = Parser.Make (S) in
  let module L = Lexer.Make (S) in
   SyntacticAnalysis.parse_string s P.patch L.main 

let patch_from_file ?(internal=false) s : AST.patch = 
  let module S = struct let internal = internal end in
  let module P = Parser.Make (S) in
  let module L = Lexer.Make (S) in
  SyntacticAnalysis.parse_file s P.patch L.main


