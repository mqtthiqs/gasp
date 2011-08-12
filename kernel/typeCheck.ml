open Util
open NLF.Utils
open NLF
 
open Definitions
type definitions = NLF.definitions

(** Debugging stuff. *)
let indent = ref (-1) 
let enter = 
  fun flag title what result ->
    if flag then begin 
      incr indent;
      Printf.printf "<%s [%d]>\n" title !indent;
      what ();
      let msg = Format.flush_str_formatter () in
      Printf.printf " === input ===\n%s\n" msg;
    end;
    let r = result () in
    if flag then begin 
      let msg = Format.flush_str_formatter () in
      Printf.printf " === output ===\n%s\n</%s [%d]>\n" msg title !indent;
      decr indent;
    end;
    r

let dbg_open_fam_flag = true
let dbg_conv_spine_flag = true
let dbg_wf_fam_flag = true
let dbg_wf_obj_flag = true
let dbg_wf_kind_flag = true

(** Typing errors. *)
type invalid_spine_reason = 
  | NotConvertible of fam * fam
  | NotAProduct
  | BadArity of bool

exception InvalidSpine of signature * env * spine * invalid_spine_reason

exception InvalidTypeAnnotation of variable * fam * fam

exception UnboundFamilyConstructor of fhead

(** Lookup for the type of a head. *)
let fam_of_head sign env = function
  | HConst c -> find_oconst c sign 
  | HVar y -> lookup_declaration y env

(** [apply_definition_construct Γ on_obj refresh_term on_term] gives a
    (parameterized) dynamic semantics to the two environment related
    constructions of the language, namely [Open (x, t)] and [Define
    (definitions, t)], as actions on their sub-term [t].

    - [on_obj Γ xa xo] where [xa] is the typing annotation associated
    to [xo] in [Γ]. It computes the local environment associated to the
    object [xo].

    - [on_term] is applied to the sub-term [t] under the local definitions
    computed from [Open] or [Define]. 
    
    - [on_definitions] is applied to the [definitions] to obtain a
    local environment. When these [definitions] come from an unchecked
    term, their typing annotations must be inferred. On the contrary,
    when these [definitions] come from an already checked term (i.e.
    a term that comes from the typing environment), then typing
    annotations are already present. Therefore, [on_definitions] will
    be instantiated with the type checker in the first case and with
    the identity function in the second case. 
*)
let apply_definition_construct env on_definitions on_obj refresh_term on_term = function
  | Open (x, t) ->
    let (xo, xa) = lookup_definition x env in
    let x_environment = on_obj env xa xo in
    on_term x_environment (env @@ x_environment) t

  | Define (definitions, t) ->
    let definitions, t = 
      Refresh.alpha_rename_define definitions refresh_term t 
    in
    let local_env = on_definitions env definitions in
    on_term local_env (env @@ local_env) t

(** [conv_spine destruct_prod conv_head sign env a l l'] *)
let rec conv_spine conv_head sign env telescope l l' =
  match telescope, l, l' with
    | _, [], [] -> 
      true

    | (x, a) :: b, h :: l, h' :: l' ->
      if conv_head sign env h h' then 
	let env = define x (head_as_obj h) a env in
	conv_spine conv_head sign env b l l'
      else 
	false

    | _ ->
      (* By well-formedness of [l] and [l']. *)
      assert false

and dbg_conv_spine conv_head sign env telescope l l' = 
  enter dbg_conv_spine_flag "conv_spine" 
    (fun () -> 
      Format.fprintf Format.str_formatter 
	"@[%a@,@ ⊢@,@ @[%a@]@ @,≡@ @[%a@] at@ @,@[%a@]@]"
	Pp.pp_environment env
	Pp.pp_spine l
	Pp.pp_spine l'
	Pp.pp_telescope telescope)
    (fun () ->
      let r = conv_spine conv_head sign env telescope l l' in
      Format.fprintf Format.str_formatter "@[%B@]" r;
      r)

(** [open_fam Δ Γ A] computes a set of definitions 
    [Δ'] and a family [B] such that:

    - [Δ' in B] is equivalent to [Δ in A] under [Γ].
    - [B] is not an environment related construct. 

*)
let rec open_fam : signature -> env -> env -> fam -> env * fam = 
  fun sign local_env env fam ->
    match fam with
      | FDef d ->
	apply_definition_construct env 
	  (fun _ x -> definitions_as_env x) (import_obj sign) Refresh.fam
	  (fun d' -> dbg_open_fam sign (local_env @@ d')) d

      | x -> 
	(local_env, x)

and dbg_open_fam sign local_env env fam = 
  enter dbg_open_fam_flag "open_fam" 
    (fun () -> 
      Format.fprintf Format.str_formatter "@[@[%a@]@, |@, @[%a@]@, ⊢@, @[%a@]@]"
	Pp.pp_environment env
	Pp.pp_environment local_env
	Pp.pp_fam fam)
    (fun () ->
      let (r_env, fam') as r = open_fam sign local_env env fam in
      Format.fprintf Format.str_formatter "@[%a@ in@ %a@]@]"
	Pp.pp_environment r_env
	Pp.pp_fam fam';
      (* The resulting family cannot be an environment-related construct. *)
      assert (match fam' with FDef _ -> false | _ -> true);
(*      assert (conv_fam sign (env @@ local_env @@ r_env) fam fam'); *)
      r)

(** [whnf_obj_spine Γ A o l] computes the weak head normal form of [t] 
    applied to [S] at type [A] under typing environment [Γ] as well
    as the local environment that is necessary to interpret [t]
    under [Γ]. 

    Preconditions: 
    - [A] is a well-formed family. 
    - [t] is a well-formed object. 
    - [l] is compatible with the expectations of [t]. 

    This implements the judgment:

         Γ ⊢ t : a ◃ l ⇓ D in t

*)
and whnf_obj_spine : signature -> env -> fam -> obj -> spine -> env * obj = 
  fun sign env a t l ->
    let (a_env, a) = dbg_open_fam sign (empty ()) env a in
    match a, t, l with
	
    (*
      x ≡ y      Γ, [x = h : ty] ⊢ o : b ◃ l ⇓ D in t
      —————————————————————————————––————————————————————————————————
      Γ ⊢ λ [x : ty]. o : π[y : ty']. b ◃ h l ⇓ [ x = h : ty ] D in t
    *)
    | FProd (y, ty', b), OLam (x, ty, o), h :: l ->
	let (x, ty, o)  = Refresh.alpha_rename_lam x ty o in
	let (y, ty', b) = Refresh.alpha_rename_prod y ~into:x ty' b in
	(* As we are working with a well-typed object, we have [ty ≡ ty']. *)
	let extra_env = (x --> (head_as_obj h)) ty in
	let (local_env, t) = whnf_obj_spine sign (env @@ extra_env) b o l	in
	(a_env @@ extra_env @@ local_env, t)
	  
    (* 
       ——————————————————————————————————————————————————
       Γ ⊢ λ [ x : ty ]. o : A ◃ • ⇓ • in λ [ x : ty ]. o
    *)
    | _, OLam _, [] -> 
      (empty (), t)

    (* 
       ———————————————————————————
       Γ ⊢ c S : A ◃ • ⇓ • in c S
    *)
    | a, OApp (HConst _, _), _ -> 
      assert (l = []);
      (empty (), t)

    | a, OApp (HVar x, l), l' ->
      let (_, xa, xt) = lookup x env in
      begin match xt with
	(* 
	   ——————————————————————————
	   Γ ⊢ x S : A ◃ • ⇓ • in x S
	*)
	| None -> assert (l' = []); (empty (), t)

	(* 
	   Γ[x] = (u : xa) 
	   Γ ⊢ u : A ◃ S l ⇓ D in u
	   ——————————————————————————
	   Γ ⊢ x S : A ◃ • ⇓ D in u
	*)
	| Some xt -> whnf_obj_spine sign env xa xt (l @ l')
      end


    (* See [apply_definition_construct]. *)
    | a, ODef d, l ->
      apply_definition_construct env (fun _ x -> definitions_as_env x) (import_obj sign) Refresh.obj
	(fun local_env env x -> 
	  let (local_env', t) = whnf_obj_spine sign env a x l in
	  (local_env @@ local_env', t)) d

    | _ -> 
      (* FIXME: To be removed. *)
      assert false (* Because obj is well-formed. *)

and whnf_obj sign env a o = 
  whnf_obj_spine sign env a o []

and dbg_whnf_obj sign env a o = 
  enter dbg_open_fam_flag "whnf_obj" 
    (fun () -> 
      Format.fprintf Format.str_formatter "@[@[%a@]@, ⊢@, @[%a@]@,:@ @[%a@]@]"
	Pp.pp_environment env
	Pp.pp_obj o
	Pp.pp_fam a)
    (fun () ->
      let (r_env, o') as r = whnf_obj sign env a o in
      Format.fprintf Format.str_formatter "@[%a@ in@ %a@]"
	Pp.pp_environment r_env
	Pp.pp_obj o';
      r)

and import_obj sign env a o = 
  fst (whnf_obj_spine sign env a o [])

and conv_obj : signature -> env -> fam -> obj -> obj -> bool =
  fun sign env a t u ->
    let t_local_env, whnf_t = whnf_obj sign env a t in
    let u_local_env, whnf_u = whnf_obj sign env a u in
    conv_whnf_obj sign (env @@ (t_local_env @@ u_local_env)) whnf_t whnf_u

and conv_whnf_obj : signature -> env -> obj -> obj -> bool = 
  fun sign env t u ->
    match t, u with
      | OApp (h1, l1), OApp (h2, l2) when h1 = h2 ->
	conv_spine conv_head sign env 
	  (telescope_of_fam_prod (fam_of_head sign env h1)) l1 l2

      | OApp (_, _), OApp (_, _) ->
	false

      | OLam (x, a, o), OApp (h, l) ->
	let env = declare x a env in
	conv_whnf_obj sign env o (OApp (h, l @ [HVar x]))

      | OLam (x, ty, o), OLam (y, ty', t) ->
	let (x, ty, o)  = Refresh.alpha_rename_lam x ty o in
	let (y, ty', t) = Refresh.alpha_rename_lam y ~into:x ty' t in
	let env = declare x ty env in
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
      conv_obj sign env xa xo (head_as_obj h)
	
and conv_fam sign env a b = 
  let a_local_env, nude_a = open_fam sign (empty ()) env a 
  and b_local_env, nude_b = open_fam sign (empty ()) env b in
  let env = env @@ (a_local_env @@ b_local_env) in

  match nude_a, nude_b with
    | FProd (x, a, b), FProd (x', a', b') ->
      let (x, a, b) = Refresh.alpha_rename_prod x a b in
      let (x', a', b') = Refresh.alpha_rename_prod x' ~into:x a' b' in
      let env' = declare x a env in
      conv_fam sign env a a' && conv_fam sign env' b b'

    | FApp (h, l), FApp (h', l') ->
      if h' <> h then 
	false
      else 
	let t = try 
	  telescope_of_kind_prod (find_fconst h sign)
	  with Not_found -> raise (UnboundFamilyConstructor h)
	in
	conv_spine conv_head sign env t l l'
    | _ ->
      false

let rec wf_spine = 
  fun destruct_a sign env a spine ->
    match spine, destruct_a a with
      | [], _ -> 
	empty (), a

      | h :: l, Some (x, a, b) ->
	let h_fam = fam_of_head sign env h in
	if not (conv_fam sign env a h_fam) then
	  raise (InvalidSpine (sign, env, spine, NotConvertible (a, h_fam)))
	else 
	  let extra_env = (x --> (head_as_obj h)) a in
	  let local_defs, a = wf_spine destruct_a sign (env @@ extra_env) b l in
	  (extra_env @@ local_defs, a)

      | _ :: _, None -> 
	raise (InvalidSpine (sign, env, spine, NotAProduct))

let rec wf_fam : signature -> env -> fam -> fam =
  fun sign env a ->
    match a with
      | FApp (h, spine) ->
	ignore (wf_spine destruct_kind_prod sign env (find_fconst h sign) spine);
	FApp (h, spine)
	  
      | FProd (x, a, b) -> 
	let (x, a, b) = Refresh.alpha_rename_prod x a b in
	let a = dbg_wf_fam sign env a in
	let b = dbg_wf_fam sign (declare x a env) b in
	FProd (x, a, b)

      | FDef (Open (x, a)) ->
	let xo, xty = lookup_definition x env in
	let x_definitions, _ = whnf_obj sign env xty xo in
	let a = dbg_wf_fam sign (env @@ x_definitions) a in
	FDef (Open (x, a))
	  
      | FDef (Define (definitions, a)) ->
	let local_definitions, a = 
	  Refresh.alpha_rename_define definitions Refresh.fam a
	in    
	let local_definitions = wf_definitions sign env local_definitions in
	let a = dbg_wf_fam sign (env @+ local_definitions) a in
	FDef (Define (local_definitions, a))

      (* This case should never appear. (see FIXME in NLF) *)
      | FConst _ -> assert false

and dbg_wf_fam sign env a = 
  enter dbg_wf_fam_flag "wf_fam" 
    (fun () ->
      Format.fprintf Format.str_formatter "@[%a@]@, ⊢@, @[%a@]@]"
	Pp.pp_environment env
	Pp.pp_fam a)
    (fun () ->
      let r = wf_fam sign env a in
      Format.fprintf Format.str_formatter "@[%a@]" Pp.pp_fam r;
      r)

and wf_obj sign env o : obj * fam =
  match o with
    | OLam (x, a, o) ->
      let (x, a, o) = Refresh.alpha_rename_lam x a o in
      let a = dbg_wf_fam sign env a in
      let (o, b) = dbg_wf_obj sign (declare x a env) o in
      (OLam (x, a, o), FProd (x, a, b))

    | OApp (h, l) ->
      let h_fam = fam_of_head sign env h in
      Format.printf "@[fam_of_head@;@[%a@]@]" 
	Pp.pp_fam h_fam;
      let h_fam_definitions, h_fam = open_fam sign (empty ()) env h_fam in
      let env = env @@ h_fam_definitions in
      let local_defs, final_fam = wf_spine destruct_fam_prod sign env h_fam l in
      (close (fun x -> ODef x) h_fam_definitions (OApp (h, l)), 
       close (fun x -> FDef x) (h_fam_definitions @@ local_defs) final_fam)

    | ODef (Open (x, o)) ->
      let xo, xty = lookup_definition x env in
      let x_definitions, _ = whnf_obj sign env xty xo in
      let o, a = dbg_wf_obj sign (env @@ x_definitions) o in
      (ODef (Open (x, o)), FDef (Open (x, a)))

    | ODef (Define (definitions, o)) ->
      let local_definitions, o = 
	Refresh.alpha_rename_define definitions Refresh.obj o 
      in    
      let local_definitions = wf_definitions sign env local_definitions in
      let o, a = dbg_wf_obj sign (env @+ local_definitions) o in
      (ODef (Define (local_definitions, o)), 
       FDef (Define (local_definitions, a)))

    | OVar _ | OConst _ ->
      assert false

and dbg_wf_obj sign env o = 
  enter dbg_wf_obj_flag "wf_obj" 
    (fun () -> 
      Format.fprintf Format.str_formatter "@[%a@]@, ⊢@, @[%a@]@]"
	Pp.pp_environment env
	Pp.pp_obj o)
    (fun () ->
      let (o', a) as r = wf_obj sign env o in
      Format.fprintf Format.str_formatter "@[@[%a@]@,:@ @[%a@]@]"
	Pp.pp_obj o'
	Pp.pp_fam a;
      r)

and wf_definitions sign env defs = 
  List.fold_left (fun (defs : definitions) (x, ty, o) ->
    match o with
      | Some o ->
	let (o, a) = dbg_wf_obj sign (env @+ defs) o in
	let ty = match ty with
	  | None -> a
	  | Some ty -> 
	    if not (conv_fam sign env ty a) then 
	      raise (InvalidTypeAnnotation (x, ty, a))
	    else 
	      ty
	in
	define x o (Some ty) defs
      | None -> 
	assert (ty != None);
	declare x ty defs) 
    (empty ()) 
    (as_list defs)

let rec wf_kind sign env = function
  | KType -> 
    KType

  | KProd (x, a, k) ->
    let (x, a, k) = Refresh.alpha_rename_kind_prod x a k in    
    let a = dbg_wf_fam sign env a in
    let env = declare x a env in 
    KProd (x, a, dbg_wf_kind sign env k)

and dbg_wf_kind sign env k =
  enter dbg_wf_kind_flag "wf_kind" 
    (fun () -> 
      Format.fprintf Format.str_formatter "@[%a@]@, ⊢@, @[%a@]@]"
	Pp.pp_environment env
	Pp.pp_kind k)
    (fun () ->
      let r = wf_kind sign env k in
      Format.fprintf Format.str_formatter "@[%a@]" Pp.pp_kind r;
      r)

let message_of_reason = function
  | NotConvertible (a, b) -> 
    Format.fprintf Format.str_formatter "@[@[%a@]@,is incompatible with@,@[%a@]@]" 
      Pp.pp_fam a Pp.pp_fam b;
    Format.flush_str_formatter ();
  | BadArity true -> 
    "too many arguments"
  | BadArity false ->
    "not enough arguments"
  | NotAProduct ->
    "not a product"

let type_checking_error msg = 
  Error.global_error "type checking" msg

let handle_error body = 
  try 
    body () 
  with 
    | InvalidTypeAnnotation (x, ty, a) -> 
      type_checking_error 
	(Format.fprintf Format.str_formatter 
	   "@[%s is expected to have type:@,@[%a@]@,\
            but the following type is given:@,@[%a@].@]"
	   (Name.of_variable x)
	   Pp.pp_fam ty
	   Pp.pp_fam a;
	 Format.flush_str_formatter ())

    | InvalidSpine (_, _, _, reason) ->
      type_checking_error (message_of_reason reason)

    | UnboundFamilyConstructor v ->
      type_checking_error 
	(Printf.sprintf "Family constructor `%s' is unbound in signature." 
	   (Name.of_fconst v))
      
let in_repo sign fam_of_repo ?repo (what : env -> _) = 
  handle_error
    (fun () -> 
      match repo with
	| None -> what (Environment.empty ())
	| Some repo -> 
	  what (fst (dbg_whnf_obj sign (Environment.empty ()) fam_of_repo repo)))
    
let obj sign fam_of_repo ?repo o = 
  in_repo sign fam_of_repo ?repo (fun env -> dbg_wf_obj sign env o)

let fam sign fam_of_repo ?repo a = 
  in_repo sign fam_of_repo ?repo (fun env -> dbg_wf_fam sign env a)

let kind sign fam_of_repo ?repo k = 
  in_repo sign fam_of_repo ?repo (fun env -> dbg_wf_kind sign env k)

   
