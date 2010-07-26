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

(** [update r context x v challenger] integrates the result [v] of the
    latest patch inside the repository by producing a new version of
    [x] that is assigned the value [v]. Every object in the repository
    [r] that is under [context] and that uses the old version of [x]
    is also updated to use the new version of [x].

    It is unlikely that the clients of the old version of [x] are also
    compatible with its new version. We rely on the metatheoretical
    side of the system (that is realized by the programmer, or any
    effective metatheorem, like a type-checker) to write an adaptation
    patch, we denote this request using a "challenge", which a patch
    with a hole to be filled. An adaptation patch may not totally
    resolve the integration problem: it may only decompose this
    problem into new integration problems. Thus, the integration
    problem is recursive and interactive. A challenge may also be
    rejected by the metatheoretical side because an adaptation patch
    cannot be written or has not been found. 

    In that case, we could try to reformulate the challenges (by
    enlarging the context for instance) or we can also fail, which
    is simpler. 
*)
let update r context x v challenger = 
  assert false
