let initial = 
  Check.infer_env Env.empty StlcdecInternalize.internalized_prelude

let save env repository_filename = 
  let ptype = Env.to_ptype env in
  let cout  = open_out repository_filename in
  Format.fprintf (Format.formatter_of_out_channel cout) 
    "@[%a@]@." Print.ptype' ptype;
  close_out cout

let load repository_filename = 
  let AST.Patch repository = 
    ASTparser.patch_from_file ~internal:true repository_filename 
  in
  Check.infer_env Env.empty repository

let initialize repository_filename = 
  let repository = initial in 
  save repository repository_filename

(** [typecheck r] checks a repository stored in file [r]. *)
let typecheck filename = 
  let (AST.Patch t) = ASTparser.patch_from_file filename in
  let s = Check.infer_type Env.empty t in
  Print.ptype Format.std_formatter (AST.Sort s);
  Format.pp_print_newline Format.std_formatter ()
