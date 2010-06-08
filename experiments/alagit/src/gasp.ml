open SyntacticAnalysis

let command = ref `None

let define_command = function
  | "init"   -> command := `Init
  | "commit" -> command := `Commit
  | _ -> assert false

let options = Arg.align 
  [
    "--debug", Arg.Set Settings.debug, 
    " Set debug mode.";

    "--do", Arg.Symbol ([ "init"; "commit" ], define_command),
    " Apply an operation on the current repository."
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

(** For the moment, we focus on the STLCdec programming language. *)

(** [internalize_stlcdec n f] internalizes a view on [name] stored in
    [filename]. *)
let internalize_stlcdec_fragment_view name filename = 
  let _fragment_ast : StlcdecAST.fragment = 
    parse_file filename StlcdecParser.fragment StlcdecLexer.main
  in
  let _fragment_patch = 
    StlcdecInternalize.fragment _fragment_ast
  in
  assert false

let initialize_stlcdec_repository () = 
  let _repository = StlcdecInternalize.initial_repository in 
  assert false

let load_stlcdec_repository () = 
  assert false

let commit_specification = "commit [rootname] [filename] [kind]"

let _ =
  match !command with
    | `None   -> List.iter typecheck arguments
    | `Init   -> initialize_stlcdec_repository ()
    | `Commit -> 
	let _repository = load_stlcdec_repository () in
	let name, filename, kind = 
	  Misc.ListExt.get3 commit_specification arguments 
	in

	(** The user demands an integration of its work in the repository. *)

	(** The provided filename is assumed to be a (modified) view
	    on something that is named [N] by the user. We internalize
	    this view "as is", with minimal syntactic requirement (the
	    view must be syntactically correct). *)

	(** The view category is deduced from the filename extension. *)
	let extension = Filename.chop_extension filename in 
	(* For the moment, we only handle one kind of view: views on
	   module fragment (which is composed of a set of parameters 
	   and a set of declarations). *)
	let _internal_name, _repository = 
	  match extension with
	    | "module" -> internalize_stlcdec_fragment_view name filename
	    | _ -> Error.global_error "during commit" "Invalid view extension."
	in
	
        (** At this point, a fresh internal name is associated to the
	    internalized version of the user module fragments. This 
	    internal representation may share some subterms with 
	    already defined terms. *)

        (** Now, we want to produce an integration patch with respect to
	    the user desiderata. *)
	match kind with
	  | "save" -> 
	      (** The user only wants to produce a snapshot of its work. *)
	      ()
	  | _ ->
	      (** The user wants a higher level of integration by updating 
		  the older version of the subtree called [name] in an
		  existing subtree A. 

		  For instance, if the fragment is used by another
		  typing derivations, then the integration patch must
		  produce a new version of A.  *)
	      assert false (* FIXME: soon. *)
	
	  

