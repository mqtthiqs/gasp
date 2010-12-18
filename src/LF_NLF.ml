open NLF 
module P = Position

module CMap = Map.Make(struct type t = LF.constant let compare = Pervasives.compare end)
type arg_position =  NLF.fam list CMap.t
  
let name_to_name = function
  | LF.Anonymous -> Anonymous
  | LF.Name x -> Name (P.value x)      

let rec obj_to_obj sign env nenv t =

  let rec constr_obj_args sign (env:LF.env) c a args eargs =
    match P.value a, args with
      | LF.FProd (LF.Name x,a,b), t :: args ->
	  constr_obj_args sign ((P.value x,a) :: env) c b args 
	    (NLFEnv.add eargs (name_to_name (LF.Name x)) (NLFEnv.ODef (obj_to_obj sign env args t)))
      | _ -> assert false
  in
  let rec args_to_obj sign env args t =
    match P.value t with
      | LF.OLam(x,a,t) -> assert false
      | LF.OApp (t,u) -> args_to_obj sign env (u::args) t
      | LF.OConst c ->
	  begin match List.assoc c sign with
	    | LF.FDecl k -> assert false	(* hoc d'objet est une famille *)
	    | LF.ODecl a -> constr_obj_args sign env c a args NLFEnv.empty
	  end
      | LF.OVar x ->
	  match List.assoc x with
	    | _ -> assert false
  in
  let rec obj_to_obj sign env (nenv:NLFEnv.t) t =
    match P.value t with
      | LF.OLam(LF.Name x,a,t) ->
	  obj_to_obj sign ((P.value x,a) :: env)
	    (NLFEnv.add nenv (name_to_name x) (NLFEnv.ODecl (fam_to_fam sign env a)))
	    t
      | _ -> args_to_obj sign env [] t
  in
  obj_to_obj sign env nenv t

and fam_to_fam sign env nenv a =
  
  let rec constr_fam_args sign env c k args eargs =
    match P.value k, args with
      | LF.KType, [] -> 
	  NLF.Fam(env, NLF.FConst(c, eargs))
      | LF.KProd(x, a, k), t :: args -> 
	  constr_fam_args sign env c k args 
	    (NLFEnv.add eargs (name_to_name x) (NLFEnv.ODef (obj_to_obj sign env t)))
      | LF.KProd _, [] -> assert false			(* pas assez d'args *)
      | LF.KType, _::_ -> assert false			(* trop d'args *)
  in
  let rec args_to_fam sign env args a =
    match P.value a with
      | LF.FProd _ -> assert false (* application à un prod *)
      | LF.FLam _  -> assert false (* FLam pas implémenté *)
      | LF.FApp (a,t) -> args_to_fam sign env (t::args) a
      | LF.FConst c -> 
	  match List.assoc c sign with
	    | LF.ODecl a -> assert false	(* hoc de famille est un objet *)
	    | LF.FDecl k -> constr_fam_args sign env c k args NLFEnv.empty
  in
  let rec fam_to_fam sign env nenv a =
    match P.value a with
      | LF.FProd (x, a, b) ->
	  fam_to_fam sign ((x,a) :: env)
	    (NLFEnv.add nenv (name_to_name x) (NLFEnv.ODecl (fam_to_fam sign env NLFEnv.empty a)))
	    b
      | _ -> args_to_fam sign env [] a
  in
  fam_to_fam sign env nenv a

let kind_to_kind sign k =

  let rec kind_to_env env k = 
    match P.value k with
      | LF.KType -> env
      | LF.KProd (x, a, k) ->
	  kind_to_env
	    (NLFEnv.add env (name_to_name x) (NLFEnv.ODecl (fam_to_fam sign env a)))
	    k
  in
  NLF.Kind (kind_to_env NLFEnv.empty k)

let sign_to_sign (s1:LF.sign) =
  Util.list_fold_left_zip
    (fun acc e s -> match e with
       | c, LF.FDecl k -> 
	   NLFSign.add acc c (NLFSign.FDecl (kind_to_kind s k))
       | c, LF.ODecl a ->
	   NLFSign.add acc c (NLFSign.ODecl (fam_to_fam s NLFEnv.empty a))
    ) NLFSign.empty [] s1
