open Util
open NLF
 
type env = (fam option, obj) Environment.t
open Definitions
type definitions = (fam option, obj) Definitions.t

let whnf_from_definition_construct env on_obj refresh_term on_term = function
  | Open (x, t) ->
    let (xo, xa) = lookup_definition x env in
    (* Every definition in the environment is annotated. *)
    let xa = unSome xa in 
    let x_definitions = on_obj env xa xo in
    on_term x_definitions (env @@ x_definitions) t

  | Define (definitions, t) ->
    let definitions, t = Refresh.alpha_rename_define definitions refresh_term t in
    on_term definitions (env @@ definitions) t


let rec whnf_obj_spine : env -> fam -> obj -> spine -> definitions * obj = 
  fun env a t l ->
    let a_definitions, a = whnf_fam (empty ()) env a in
    match a, t, l with
    | FProd (y, ty', b), OLam (x, ty, o), h :: l ->
      let (x, ty, o)  = Refresh.alpha_rename_lam x ty o in
      let (y, ty', b) = Refresh.alpha_rename_prod y ~into:x ty' b in
      (* If we are working with a well-typed object, then [ty == ty']. *)
      let definition = (x --> (head_as_obj h)) (Some ty) in
      let (definitions, t) = 
	whnf_obj_spine (env @@ definition) b o l
      in
      (a_definitions @@ definition @@ definitions, t)

    | _, OLam _, [] -> 
      (empty (), t)

    | a, ODef d, l ->
      whnf_from_definition_construct env import_obj Refresh.obj
	(fun defs env x -> 
	  let (definitions, t) = whnf_obj_spine env a x l in
	  (defs @@ definitions, t)) d

    | a, OApp (HConst _, _), _ -> 
      (empty (), t)

    | a, OApp (HVar x, l), l' ->
      let (_, xa, xt) = lookup x env in
      let xa = unSome xa in
      begin match xt with
	| None -> (empty (), t)
	| Some xt -> whnf_obj_spine env xa xt (l @ l')
      end

    | _ -> 
      (* FIXME: To be removed. *)
      assert false (* Because obj is well-formed. *)

and whnf_obj env a o = whnf_obj_spine env a o []

and whnf_fam : definitions -> env -> fam -> definitions * fam = 
  fun defs env fam ->
    match fam with
      | FDef d ->
	whnf_from_definition_construct env import_obj Refresh.fam
	  (fun d' -> whnf_fam (defs @@ d')) d

      | x -> 
	(defs, x)

and import_obj env a o = 
  fst (whnf_obj_spine env a o [])

let fam_of_head sign env = function
  | HConst c -> find_oconst c sign 
  | HVar y -> 
    match lookup_declaration y env with
      | Some ty -> ty
      | None -> 
	(* Every head declared in the environment as type annotation. *)
	assert false

let destruct_fam_prod = function
  | FProd (x, a, b) -> 
    let (x, a, b) = Refresh.alpha_rename_prod x a b in
    Some (x, a, b)
  | _ -> None

let destruct_kind_prod = function
  | KProd (x, a, k) -> 
    let (x, a, k) = Refresh.alpha_rename_kind_prod x a k in
    Some (x, a, k)
  | _ -> None

let rec conv_spine destruct_prod conv_head sign env a l l' =
  match destruct_prod a, l, l' with
    | _, [], [] -> 
      true

    | Some (x, a, b), h :: l, h' :: l' ->
      if conv_head sign env h h' then 
	let env = define x (head_as_obj h) (Some a) env in
	conv_spine destruct_prod conv_head sign env b l l'
      else 
	false
    | _ ->
      (* By well-formedness of [l] and [l']. *)
      assert false


let rec conv_obj : signature -> env -> fam -> obj -> obj -> bool =
  fun sign env a t u ->
    let t_definitions, whnf_t = whnf_obj env a t in
    let u_definitions, whnf_u = whnf_obj env a u in
    conv_whnf_obj sign (env @@ t_definitions @@ u_definitions) whnf_t whnf_u

and conv_whnf_obj : signature -> env -> obj -> obj -> bool = 
  fun sign env t u ->
    match t, u with
      | OApp (h1, l1), OApp (h2, l2) when h1 = h2 ->
	conv_spine destruct_fam_prod conv_head sign env (fam_of_head sign env h1) l1 l2

      | OApp (_, _), OApp (_, _) ->
	false

      | OLam (x, a, o), OApp (h, l) ->
	let env = declare x (Some a) env in
	conv_whnf_obj sign env o (OApp (h, l @ [HVar x]))

      | OLam (x, ty, o), OLam (y, ty', t) ->
	let (x, ty, o)  = Refresh.alpha_rename_lam x ty o in
	let (y, ty', t) = Refresh.alpha_rename_lam y ~into:x ty' t in
	let env = declare x (Some ty) env in
	conv_whnf_obj sign env o t
	  
      | _ -> 
      (* Because 't' is whnf. *)
	assert false
	  
and conv_head sign env h h' = 
  match h, h' with
    | HConst c, HConst c' -> 
      c = c'
    | HVar x, HVar y when x = y -> 
      true
    | HVar x, h | h, HVar x ->
      let (xo, xa) = lookup_definition x env in
      conv_obj sign env (unSome xa) xo (head_as_obj h)
	
and conv_fam sign env a b = 
  let a_definitions, nude_a = whnf_fam (empty ()) env a 
  and b_definitions, nude_b = whnf_fam (empty ()) env b in
  let env = env @@ a_definitions @@ b_definitions in
  match nude_a, nude_b with
    | FProd (x, a, b), FProd (x', a', b') ->
      let (x, a, b) = Refresh.alpha_rename_prod x a b in
      let (x', a', b') = Refresh.alpha_rename_prod x' ~into:x a' b' in
      let env' = declare x (Some a) env in
      conv_fam sign env a a' && conv_fam sign env' b b'
    | FApp (h, l), FApp (h', l') ->
      if h' <> h then 
	false
      else 
	conv_spine destruct_kind_prod conv_head sign env (find_fconst h sign) l l'
    | _ ->
      assert false

type invalid_spine_reason = 
  | NotConvertible of fam * fam
  | BadArity of bool

exception InvalidSpine of signature * env * spine * invalid_spine_reason

let rec wf_spine = 
fun destruct_a sign env a spine ->
  match spine, destruct_a a with
    | [], _ -> 
      (empty (), a)

    | h :: l, Some (x, a, b) ->
      let h_fam = fam_of_head sign env h in
      if not (conv_fam sign env a h_fam) then
	raise (InvalidSpine (sign, env, spine, NotConvertible (a, h_fam)))
      else 
	let definition = (x --> (head_as_obj h)) (Some a) in
	let (definitions, final_fam) = 
	  wf_spine destruct_a sign (env @@ definition) b spine 
	in
	(definition @@ definitions, final_fam)
	  
    | _ ->
      raise (InvalidSpine (sign, env, spine, BadArity (spine = [])))

let rec wf_fam : signature -> env -> fam -> definitions * fam =
  fun sign env a ->
    let a_definitions, nude_a = whnf_fam (empty ()) env a in
    let definitions, wf_nude_a = 
      match nude_a with
	| FApp (h, spine) ->
	  let (spine_definitions, _) = 
	    wf_spine destruct_kind_prod sign env (find_fconst h sign) spine
	  in
	  (spine_definitions, FApp (h, spine))

	| FProd (x, a, b) -> 
	  let (x, a, b) = Refresh.alpha_rename_prod x a b in
	  let a_definitions, a = wf_fam sign env a in
	  let b_definitions, b = wf_fam sign (declare x (Some a) env) b in
	  (a_definitions @@ b_definitions, FProd (x, a, b))

      (* This case should never appear. (see FIXME in NLF) *)
	| FConst _ -> assert false

      (* This case should not appear thanks to [whnf_fam]. *)
	| FDef _ -> assert false
    in
    (a_definitions @@ definitions, wf_nude_a)

and wf_obj sign env = function
  | OLam (x, a, o) ->
    let (x, a, o) = Refresh.alpha_rename_lam x a o in
    let a_definitions, a = wf_fam sign env a in
    let (definitions, o, b) = wf_obj sign (declare x (Some a) env) o in
    (* FIXME: Here we duplicate "definitions". There should be some way    *)
    (* FIXME: to share it by returning something like:                     *)
    (* FIXME: (f = \x. definitions in o), f, {x} y = f x in open y in b    *)
    (* FIXME: Yet, I do not know which annotation should be given on [f]'s *)
    (* FIXME: definition...                                                *)
    (a_definitions, 
     OLam (x, a, ODef (Define (definitions, o))), 
     FDef (Define (definitions, b)))

  | OApp (h, l) ->
    let h_fam = fam_of_head sign env h in
    let h_fam_definitions, h_fam = whnf_fam (empty ()) env h_fam in
    let env = env @@ h_fam_definitions in
    let (l_definitions, final_fam) = wf_spine destruct_fam_prod sign env h_fam l in
    (h_fam_definitions @@ l_definitions, OApp (h, l), final_fam)

  | ODef (Open (x, o)) ->
    let xo, xty = lookup_definition x env in
    let x_definitions, _ = whnf_obj env (unSome xty) xo in
    let o_definitions, o, a = wf_obj sign (env @@ x_definitions) o in
    (o_definitions, ODef (Open (x, o)), a)

  | ODef (Define (definitions, o)) ->
    let local_definitions, o = 
      Refresh.alpha_rename_define definitions Refresh.obj o 
    in    
    let o_definitions, o, a = wf_obj sign (env @@ local_definitions) o in
    (local_definitions @@ o_definitions, o, a)

  | OVar _ | OConst _ ->
    assert false

let in_repo fam_of_repo repo what = 
  what (fst (whnf_obj (empty ()) fam_of_repo repo))
    
let obj sign fam_of_repo repo o = 
  assert false

let fam sign fam_of_repo repo f = 
  assert false

let kind sign fam_of_repo repo o = 
  assert false

   
