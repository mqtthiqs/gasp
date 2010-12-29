open Name
open NLF

(* From XLFe to NLF: environment building and forgetting arguments
   positions *)

let rec obj env = function
  | XLFe.OLam (x,a,t) -> 
      let a = fam (NLFEnv.clear env) a in
      obj (NLFEnv.add env x (NLFEnv.ODecl a)) t
  | XLFe.OHead h -> 
      let (ht, ha) = ohead env h in
      NLF.Obj(env, ht, ha)

and args env l = 
  List.fold_left
    ( fun env (x,t) ->
	let t = obj (NLFEnv.clear env) t in
	NLFEnv.add env x (NLFEnv.ODef t)
    ) (NLFEnv.clear env) l

and ohead env = function
  | XLFe.OVar(x,l,a) -> NLF.OVar(x, args env l), fhead env a
  | XLFe.OConst(c,l,a) -> NLF.OConst(c, args env l), fhead env a
  | XLFe.OApp(t,l,a) -> 
      let t = obj (NLFEnv.clear env) t in
      NLF.OApp(t, args env l), fhead env a

and fam env = function
  | XLFe.FProd (x,a,b) -> 
      let a = fam (NLFEnv.clear env) a in
      fam (NLFEnv.add env x (NLFEnv.ODecl a)) b
  | XLFe.FHead h -> 
      NLF.Fam(env, fhead env h)

and fhead env = function
  | XLFe.FConst(c,l,XLFe.KType) -> NLF.FConst(c, args env l)

let rec kind env = function
  | XLFe.KProd(x,a,k) -> 
      let a = fam (NLFEnv.clear env) a in
      kind (NLFEnv.add env x (NLFEnv.ODecl a)) k
  | XLFe.KHead(XLFe.KType) ->
      NLF.Kind env

let rec sign s = function
  | [] -> s
  | (c, XLFe.FDecl k) :: tl ->
      let k = kind NLFEnv.empty k in
      sign (NLFSign.add s c (NLFSign.FDecl k)) tl
  | (c, XLFe.ODecl a) :: tl ->
      let a = fam NLFEnv.empty a in
      sign (NLFSign.add s c (NLFSign.ODecl a)) tl

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
    
and from_app args =
  NLFEnv.fold
    (fun x e acc -> match e with
       | NLFEnv.ODecl a -> assert false
       | NLFEnv.ODef t ->  (x,from_obj t) :: acc
    ) args []

and from_fam = function 
  | NLF.Fam (env, ha) ->
      from_env_fam env (XLFe.FHead(from_fhead ha))

and from_fhead = function
  | NLF.FConst(c,args) -> XLFe.FConst(c, from_app args, XLFe.KType)

and from_ohead ha = function
  | NLF.OVar (x, args) -> XLFe.OVar(x, from_app args, from_fhead ha)
  | NLF.OConst (c, args) -> XLFe.OConst(c, from_app args, from_fhead ha)
  | NLF.OApp (t, args) -> XLFe.OApp(from_obj t, from_app args, from_fhead ha)

and from_obj = function 
  | NLF.Obj (env, ht, ha) ->
      from_env_obj env (XLFe.OHead(from_ohead ha ht))

let rec from_kind = function
  | NLF.Kind env -> from_env_kind env (XLFe.KHead(XLFe.KType))

let rec from_sign (s:NLF.sign) : XLFe.sign = 
  NLFSign.fold 
    (fun c e acc -> match e with
       | NLFSign.FDecl k -> (c, XLFe.FDecl (from_kind k)) :: acc
       | NLFSign.ODecl a -> (c, XLFe.ODecl (from_fam a)) :: acc
    ) s []
