open SyntacticAnalysis

let command = ref `None

let repository_filename = ref "repository"

let define_command = function
  | "init"   -> command := `Init
  | "commit" -> command := `Commit
  | _ -> assert false

let options = Arg.align 
  [
    "--debug", Arg.Set Settings.debug, 
    " Set debug mode.";

    "--do", Arg.Symbol ([ "init"; "commit" ], define_command),
    " Apply an operation on the current repository.";

    "--repository", Arg.Set_string repository_filename,
    " Set the file where the repository is stored. [default=repository]"
  ]

let usage_msg =
  Printf.sprintf "%s:" (Filename.basename Sys.executable_name)

let arguments =
  let arguments = ref [] in
    Arg.parse options (fun f -> arguments := f :: !arguments) usage_msg;
    !arguments

(** [typecheck r] checks a repository stored in file [r]. *)
let typecheck filename = 
  let (AST.Patch t) = parse_file filename Parser.patch Lexer.main in
  let s = Check.infer_type Env.empty t in
  Print.ptype Format.std_formatter (AST.Sort s);
  Format.pp_print_newline Format.std_formatter ()

let commit_specification = "commit [rootname] [filename] [kind]"

(* Note: We only focus on the STLCdec programming language for the
   moment. *)

let _ =
  match !command with
    | `None   -> List.iter typecheck arguments
    | `Init   -> StlcdecRepository.initialize !repository_filename
    | `Commit -> 
	let repository = StlcdecRepository.load !repository_filename in
	let name, filename, kind = 
	  Misc.ListExt.get3 commit_specification arguments 
	in

	(** The user demands an integration of its work in the repository. *)

	(** The provided filename is assumed to be a (modified) view
	    on something that is named [N] by the user. We internalize
	    this view "as is", with minimal requirement (the view must
	    only be syntactically correct). *)

	(** The view category is deduced from the filename extension. *)
	let extension = Misc.FilenameExt.get_extension filename in 
	(* For the moment, we only handle one kind of view: views on
	   module fragment (which is composed of a set of parameters 
	   and a set of declarations). In the future, we will have 
	   others views like type derivations views (i.e. signatures) 
	   for instance. *)
	let _internal_name, AST.Patch p = 
	  match extension with
	    | "module" -> StlcdecInternalize.named_fragment_view_from_file name filename
	    | _ -> Error.global_error "during commit" 
		(Printf.sprintf "Invalid view extension `%s'." extension)
	in
	Print.ptype' Format.std_formatter p;
	Format.pp_print_newline Format.std_formatter ();
	

	let repository = Check.infer_env repository p in

        (** At this point, a fresh internal name is associated to the
	    internalized version of the user module fragments. This 
	    internal representation may share some subterms with 
	    already defined terms. *)

        (** Now, we want to produce an integration patch with respect to
	    the user desiderata. *)
	match kind with
	  | "save" -> 
	      (** The user only wants to produce a snapshot of its work. *)
	      StlcdecRepository.save repository !repository_filename 

	  | _ ->
	      (** The user wants a higher level of integration by updating 
		  the older version of the subtree called [name] in an
		  existing subtree A. 

		  For instance, if the fragment is used by another
		  typing derivations, then the integration patch must
		  produce a new version of A.  *)
	      assert false (* FIXME: soon. *)
	
	  

