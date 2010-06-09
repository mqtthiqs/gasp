let patch_from_string s : AST.patch = 
  SyntacticAnalysis.parse_string s Parser.patch Lexer.main 

let patch_from_file s : AST.patch = 
  SyntacticAnalysis.parse_file s Parser.patch Lexer.main

