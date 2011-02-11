open Name
open NLF

(* From XLFe to NLF: environment building and forgetting arguments
   positions *)

let rec obj env = function
  | XLFe.OLam (x,a,t) -> 
      let a = fam (NLFEnv.clear env) a in
      obj (NLFEnv.add env x (NLFEnv.ODecl a)) t
  | XLFe.OHead h -> 
      let h, args, a, fargs = ohead env h in
      NLF.Obj(env, h, args, a, fargs)

and args env l = 
  List.fold_left
    ( fun env (x,t) ->
	let t = obj (NLFEnv.clear env) t in
	NLFEnv.add env x (NLFEnv.ODef t)
    ) (NLFEnv.clear env) l

and ohead env = function
  | XLFe.OVar(x,l,XLFe.FConst(a,l',XLFe.KType)) 
  | XLFe.OMeta(x,l,XLFe.FConst(a,l',XLFe.KType)) -> (* A meta in XLFe was actually an NLF variable *)
      let args' = args env l in
      let fargs' = args (NLFEnv.clear args') l' in
      NLF.HVar x, args', a, fargs'
  | XLFe.OConst(c,l,XLFe.FConst(a,l',XLFe.KType)) -> 
      let args' = args env l in
      let fargs' = args (NLFEnv.clear args') l' in
      NLF.HConst c, args', a, fargs'
  | XLFe.OApp(t,l,XLFe.FConst(a,l',XLFe.KType)) -> 
      let t = obj (NLFEnv.clear env) t in
      let args' = args env l in
      let fargs' = args (NLFEnv.clear args') l' in
      NLF.HObj t, args', a, fargs'

and fam env = function
  | XLFe.FProd (x,a,b) -> 
      let a = fam (NLFEnv.clear env) a in
      fam (NLFEnv.add env x (NLFEnv.ODecl a)) b
  | XLFe.FHead (XLFe.FConst(a,l,XLFe.KType)) -> 
      let fargs = args env l in
      NLF.Fam(env, a, fargs)

let rec kind env = function
  | XLFe.KProd(x,a,k) -> 
      let a = fam (NLFEnv.clear env) a in
      kind (NLFEnv.add env x (NLFEnv.ODecl a)) k
  | XLFe.KHead(XLFe.KType) ->
      NLF.KType env

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
      from_env_fam env (XLFe.FHead(XLFe.FConst(a,from_env_args args,XLFe.KType)))

and from_head args ha = function
  | NLF.HVar x -> XLFe.OVar(x, args, ha)
  | NLF.HConst c -> XLFe.OConst(c, args, ha)
  | NLF.HObj t -> XLFe.OApp(from_obj t, args, ha)

and from_obj = function 
  | NLF.Obj (env, h, args, a, fargs) ->
      let ha = XLFe.FConst(a, from_env_args fargs, XLFe.KType) in
      let args = from_env_args args in
      from_env_obj env (XLFe.OHead(from_head args ha h))

let rec from_kind = function
  | NLF.KType env -> from_env_kind env (XLFe.KHead(XLFe.KType))

let rec from_sign (s:NLF.sign) : XLFe.sign = 
  List.rev 
    (NLFSign.fold 
       (fun c e acc -> match e with
	  | NLFSign.FDecl k -> (c, XLFe.FDecl (from_kind k)) :: acc
	  | NLFSign.ODecl a -> (c, XLFe.ODecl (from_fam a)) :: acc
       ) s []
    )
