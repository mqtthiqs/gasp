open Name
open NLF

(* From XLFe to NLF: environment building and forgetting arguments
   positions *)

let rec obj env = function
  | XLFe.OLam (x,a,t) -> 
      let a = fam (NLFEnv.clear env) a in
      obj (NLFEnv.add env x (NLFEnv.ODecl a)) t
  | XLFe.OHead (h,l,XLFe.FConst(a,l')) -> 
      let args' = args env l in
      let fargs' = args (NLFEnv.clear args') l' in
      let h = ohead env h in
      NLF.Obj(env, h, args', a, fargs')

and args env l = 
  List.fold_left
    ( fun env (x,t) ->
	let t = obj (NLFEnv.clear env) t in
	NLFEnv.add env x (NLFEnv.ODef t)
    ) (NLFEnv.clear env) l

and ohead env = function
  | XLFe.HVar x
  | XLFe.HMeta x -> (* A meta in XLFe was actually an NLF variable *)
      NLF.HVar x
  | XLFe.HConst c -> NLF.HConst c
  | XLFe.HApp t -> NLF.HObj (obj (NLFEnv.clear env) t)

and fam env = function
  | XLFe.FProd (x,a,b) -> 
      let a = fam (NLFEnv.clear env) a in
      fam (NLFEnv.add env x (NLFEnv.ODecl a)) b
  | XLFe.FHead (XLFe.FConst(a,l)) -> 
      let fargs = args env l in
      NLF.Fam(env, a, fargs)

let rec kind env = function
  | XLFe.KProd(x,a,k) -> 
      let a = fam (NLFEnv.clear env) a in
      kind (NLFEnv.add env x (NLFEnv.ODecl a)) k
  | XLFe.KType -> NLF.KType env

let entry kont nlfs = function 
    | XLFe.ODecl a -> kont nlfs (NLFSign.ODecl (fam NLFEnv.empty a))
    | XLFe.FDecl k -> kont nlfs (NLFSign.FDecl (kind NLFEnv.empty k))

(* ...and back *)

let rec from_env_kind env k =
  NLFEnv.fold
    (fun x e acc -> match e with
       | NLFEnv.ODecl a -> XLFe.KProd(x, from_fam a, acc)
       | NLFEnv.ODef t -> assert false
    ) env k

and from_env_fam env k =
  NLFEnv.fold
    (fun x e acc -> match e with
       | NLFEnv.ODecl a -> XLFe.FProd(x, from_fam a, acc)
       | NLFEnv.ODef t -> assert false
    ) env k

and from_env_obj env t =
  NLFEnv.fold
    (fun x e acc -> match e with
       | NLFEnv.ODecl a -> XLFe.OLam(x, from_fam a, acc)
       | NLFEnv.ODef t -> assert false
    ) env t
    
and from_env_args args =
  NLFEnv.fold
    (fun x e acc -> match e with
       | NLFEnv.ODecl a -> assert false
       | NLFEnv.ODef t ->  (x,from_obj t) :: acc
    ) args []

and from_fam = function 
  | NLF.Fam (env, a, args) ->
      from_env_fam env (XLFe.FHead(XLFe.FConst(a,from_env_args args)))

and from_head = function
  | NLF.HVar x -> XLFe.HVar x
  | NLF.HConst c -> XLFe.HConst c
  | NLF.HObj t -> XLFe.HApp(from_obj t)

and from_obj = function 
  | NLF.Obj (env, h, args, a, fargs) ->
      let ha = XLFe.FConst(a, from_env_args fargs) in
      let args = from_env_args args in
      from_env_obj env (XLFe.OHead(from_head h, args, ha))

let rec from_kind = function
  | NLF.KType env -> from_env_kind env XLFe.KType

let rec from_sign (s:NLF.sign) : XLFe.sign = 
  List.rev 
    (NLFSign.fold 
       (fun c e acc -> match e with
	  | NLFSign.FDecl k -> (c, XLFe.FDecl (from_kind k)) :: acc
	  | NLFSign.ODecl a -> (c, XLFe.ODecl (from_fam a)) :: acc
       ) s []
    )
