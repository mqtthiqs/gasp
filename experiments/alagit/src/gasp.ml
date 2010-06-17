open SyntacticAnalysis

let command = ref `None

let repository_filename = ref "repository"

let define_command = function
  | "init"     -> command := `Init
  | "commit"   -> command := `Commit
  | "checkout" -> command := `Checkout
  | _ -> assert false

let options = Arg.align 
  [
    "--debug", Arg.Set Settings.debug, 
    " Set debug mode.";

    "--do", Arg.Symbol ([ "init"; "commit"; "checkout" ], define_command),
    " Apply an operation on the current repository.";

    "--repository", Arg.Set_string repository_filename,
    " Set the file where the repository is stored. [default=repository]"
  ]

let usage_msg =
  Printf.sprintf "%s:" (Filename.basename Sys.executable_name)

let arguments =
  let arguments = ref [] in
  Arg.parse options (fun f -> arguments := f :: !arguments) usage_msg;
  List.rev !arguments


let commit_specification   = "commit [rootname] [filename] [kind]"
let checkout_specification = "checkout [rootname] [filename] [kind]"

(* Note: We only focus on the STLCdec programming language for the
   moment. *)

let show_repository step repository = 
  if !Settings.debug then begin
    Format.printf "\
    ========================\n 
    %s\n\ 
    ========================" step;
    Print.ptype' Format.std_formatter (Env.to_ptype repository);
    Format.pp_print_newline Format.std_formatter ()
  end

let _ =
  match !command with
    | `None   -> List.iter StlcdecRepository.typecheck arguments
    | `Init   -> StlcdecRepository.initialize !repository_filename
    | `Checkout -> 
	let repository = StlcdecRepository.load !repository_filename in
	show_repository "after loading" repository;

	let name, filename = 
	  Misc.ListExt.get2 checkout_specification arguments 
	in
	let extension = Misc.FilenameExt.get_extension filename in
	(** The user is asking for a concrete view of an existing 
	    entry [name] of the repository. *)

	(** First, extract the externalized version of the entry
	    [name]. We cannot determine the exact type of this 
	    object. However, we rely on the user to provide a 
	    concrete view name [extension] which will give us enough 
	    dynamic information to type-check our code. *)
	let saver =
	  match extension with
	    | "raw-fragment" -> 
		(* For the moment, we are working with raw syntax. *)
		let entry = try 
		  Env.lookup_latest_with_prefix repository name 
		with Not_found -> 
		  Error.global_error "during checkout" 
		    (Printf.sprintf "There is no recent entry named `%s'." name)
		in
		let ast = StlcdecExternalize.fragment_view repository entry in
		(Misc.FormatExt.save StlcdecPrint.fragment ast)
	    | _ -> 
		Error.global_error "during checkout" 
		  (Printf.sprintf "Invalid view extension `%s'." extension)
		
	in
	(** The provided filename denotes the container of this view. 
	    If a file already exists with this name, we first clone
	    it in a hidden backup file. *)
	let cout = Misc.IOExt.open_out_with_backup filename in
	saver cout;
	close_out cout

    | `Commit -> 
	let repository = StlcdecRepository.load !repository_filename in
	let name, filename, kind = 
	  Misc.ListExt.get3 commit_specification arguments 
	in
	show_repository "after loading" repository;

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
	
	let repository = Check.infer_env repository p in
	show_repository "after commit" repository;
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

	  | "update" ->
	      (** The user wants a higher level of integration by updating 
		  the older version of the subtree called [name] in an
		  existing subtree A. 

		  For instance, if the fragment is used by another
		  typing derivation, then the integration patch must
		  produce a new version of A.  *)
	      assert false (* FIXME: soon. *)
	
	  | cmd -> 
	      Error.global_error "During integration" 
		(Printf.sprintf "Unknown integration kind `%s'." cmd)

	  

