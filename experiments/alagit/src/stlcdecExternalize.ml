open StlcdecAST
open StlcdecInternalize

exception NotEnoughKeys

let ( ! ) x = fun ks -> assert (ks = []); x

let export_from (env : Env.t) = 
  let on_name    x = Externalize.on_name env x in
  let from_names x = Externalize.from_names env x in
  let match_key  = Externalize.match_key in

  let rec declaration k = 
    match_key k 
      [
	dvalue_declaration_iname, 
	(from_names identifier' 
	   (fun x -> from_names ty' 
		(fun t -> from_names expression' 
		   (fun e -> ! (DValue (x, t, e))))));
	
	dtype_declaration_iname,
	(from_names type_identifier' (fun x -> ! (DType x)))
      ]

  and declarations k = 
    match_key k 
      [
	empty_declarations_iname, 
	(! EmptyDeclarations);
	
	cons_declarations_iname, 
	(from_names declaration' 
	   (fun x -> from_names declarations' (fun xs -> ! (ConsDeclaration (x, xs)))))
      ]

  and expression k = 
    match_key k 
      [
	var_exp_iname, 
	(from_names identifier' (fun x -> ! (Var x)));
	
	lam_exp_iname,
	(from_names identifier' 
	   (fun x -> from_names ty' 
	      (fun t -> from_names expression'
		 (fun e -> ! (Lam (x, t, e))))));

	app_exp_iname,
	(from_names expression'
	   (fun e1 -> from_names expression'
	      (fun e2 -> ! (App (e1, e2)))))
      ]

  and ty k = 
    match_key k 
      [
	var_ty_iname, 
	(from_names type_identifier' (fun x -> ! (TyVar x)));

	arrow_ty_iname,
	(from_names ty'
	   (fun ty1 -> from_names ty'
	      (fun ty2 -> ! (TyArrow (ty1, ty2)))))
      ]

  and from_literal_name n = 
    let s = Name.to_string n in
    (* FIXME: Implement clean escaping. *)
    ignore (Str.string_match (Str.regexp "data\\[\\([^]]*\\)\\]") s 0);
    try 
      ! (Str.matched_group 1 s)
    with Not_found -> assert false

  and identifier k = 
    from_literal_name k

  and type_identifier k = 
    from_literal_name k

  and typing_environment k ks = 
    Env (bindings' k ks)

  and bindings k = 
    match_key k 
      [
	nil_environment_iname, (! NoBinding);

	cons_environment_iname,
	(from_names binding' 
	   (fun x -> from_names bindings' (fun xs -> ! (ConsBinding (x, xs)))))
      ]
      
  and binding k = 
    match_key k 
      [
	bind_var_iname,
	(from_names identifier' 
	   (fun x -> from_names ty' 
	      (fun t -> ! (BindVar (x, t)))));

	bind_tyvar_iname,
	(from_names type_identifier'
	   (fun x -> ! (BindTyVar x)))
      ]

  and fragment_view k = 
    match_key k 
      [
	fragment_ctor_iname,
	(from_names typing_environment'
	   (fun env -> from_names declarations'
	      (fun decs -> ! (Fragment (env, decs)))))
      ]

  and bindings'           x ks = MetaExternalize.on env bindings (x, ks)
  and binding'            x ks = MetaExternalize.on env binding (x, ks)
  and expression'         x ks = MetaExternalize.on env expression (x, ks)
  and ty'                 x ks = MetaExternalize.on env ty (x, ks)
  and type_identifier'    x ks = MetaExternalize.on env type_identifier (x, ks)
  and identifier'         x ks = MetaExternalize.on env identifier (x, ks)
  and typing_environment' x ks = MetaExternalize.on env typing_environment (x, ks)
  and declarations'       x ks = MetaExternalize.on env declarations (x, ks)
  and declaration'        x ks = MetaExternalize.on env declaration (x, ks)
      
  in
  (on_name declaration, 
   on_name declarations, 
   on_name ty, 
   on_name identifier, 
   on_name type_identifier,
   on_name typing_environment, 
   on_name bindings, 
   on_name binding, 
   on_name fragment_view)

let fragment_view env = 
  let _, _, _, _, _, _, _, _, f = export_from env in f



