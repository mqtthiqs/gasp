let initial = 
  Check.infer_env Env.empty StlcdecInternalize.internalized_prelude

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
  let repository = initial in 
  save repository repository_filename
