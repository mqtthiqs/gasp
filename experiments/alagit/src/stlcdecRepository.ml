(** [internalize_stlcdec n f] internalizes a view on [name] stored in
    [filename]. *)
let internalize_fragment_view name filename = 
  let _fragment_ast : StlcdecAST.fragment = 
    SyntacticAnalysis.parse_file filename StlcdecParser.fragment StlcdecLexer.main
  in
  let _fragment_patch = 
    StlcdecInternalize.fragment _fragment_ast
  in
  assert false

let save env repository_filename = 
  let ptype = Env.to_ptype env in
  let cout  = open_out repository_filename in
  Format.fprintf (Format.formatter_of_out_channel cout) 
    "@[%a@]@." Print.ptype' ptype;
  close_out cout

let load repository_filename = 
  let AST.Patch repository = ASTparser.patch_from_file repository_filename in
  Check.infer_env Env.empty repository

let initialize repository_filename = 
  let repository = StlcdecInternalize.initial_repository in 
  save repository repository_filename
