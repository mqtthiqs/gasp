open StlcdecAST
open StlcdecInternalize

exception NotEnoughKeys

let ( ! ) x = fun ks -> assert (ks = []); x

let export_from (env : Env.t) = 
  let subnames k = 
    try 
      Env.subnames env k 
    with Not_found -> 
      Error.global_error "during externalization"
	(Printf.sprintf "External name `%s' is unbound." 
	   (Name.to_string k))
  in
  let on_name f k = 
    match subnames k with
      | [] -> f k [] 
      | g :: xs -> f g xs
  in
  let from_names f g = function
    | [] -> raise NotEnoughKeys
    | k :: ks -> 
	try 
	  let y = f k (subnames k) in g y ks
	with Not_found -> 
	  Error.global_error "during externalization"
	    (Printf.sprintf "External name `%s' is unbound." 
	       (Name.to_string k))
  in
  
  let match_key k l = 
    try 
      List.assoc k l
    with Not_found -> 
      Error.global_error "during externalization"
	(Printf.sprintf "Key `%s' is not in { %s }" 
	   (Name.to_string k)
	   (String.concat " " (List.map (fun (k, _) -> Name.to_string k) l))
	)

  in

  let rec declaration k = 
    match_key k 
      [
	dvalue_declaration_iname, 
	(from_names identifier 
	   (fun x -> from_names ty 
		(fun t -> from_names expression 
		   (fun e -> ! (DValue (x, t, e))))));
	
	dtype_declaration_iname,
	(from_names type_identifier (fun x -> ! (DType x)))
      ]

  and declarations k = 
    match_key k 
      [
	empty_declarations_iname, 
	(! []);
	
	cons_declarations_iname, 
	(from_names declaration 
	   (fun x -> from_names declarations (fun xs -> ! (x :: xs))))
      ]

  and expression k = 
    match_key k 
      [
	var_exp_iname, 
	(from_names identifier (fun x -> ! (Var x)));
	
	lam_exp_iname,
	(from_names identifier 
	   (fun x -> from_names ty 
	      (fun t -> from_names expression
		 (fun e -> ! (Lam (x, t, e))))));

	app_exp_iname,
	(from_names expression
	   (fun e1 -> from_names expression
	      (fun e2 -> ! (App (e1, e2)))))
      ]

  and ty k = 
    match_key k 
      [
	var_ty_iname, 
	(from_names type_identifier (fun x -> ! (TyVar x)));

	arrow_ty_iname,
	(from_names ty
	   (fun ty1 -> from_names ty
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

  and typing_environment k = 
    bindings k

  and bindings k = 
    match_key k 
      [
	nil_environment_iname, (! []);

	cons_declarations_iname,
	(from_names binding 
	   (fun x -> from_names bindings (fun xs -> ! (x :: xs))))
      ]
      
  and binding k = 
    match_key k 
      [
	bind_var_iname,
	(from_names identifier 
	   (fun x -> from_names ty 
	      (fun t -> ! (BindVar (x, t)))));

	bind_tyvar_iname,
	(from_names type_identifier
	   (fun x -> ! (BindTyVar x)))
      ]

  and fragment_view k = 
    match_key k 
      [
	fragment_ctor_iname,
	(from_names typing_environment
	   (fun env -> from_names declarations
	      (fun decs -> ! (Fragment (Env env, decs)))))
      ]
      
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



