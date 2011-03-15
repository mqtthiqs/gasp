open Name
open NLF

module E = NLFEnv
module S = NLFSubst
module A = NLFArgs

let env_add x a = function
  | NLF.Obj(e,s,h,l,c,m) -> NLF.Obj(E.add x a e,s,h,l,c,m)
  | NLF.OMeta(e,s,d,c,m) -> NLF.OMeta(E.add x a e,s,d,c,m)
let env_of = function
  | NLF.Obj(e,s,_,_,_,_)
  | NLF.OMeta(e,s,_,_,_) -> e
let subst_of = function
  | NLF.Obj(e,s,_,_,_,_)
  | NLF.OMeta(e,s,_,_,_) -> s
let with_subst s = function
  | NLF.Obj(e,_,h,args,c,fargs) -> NLF.Obj(e,s,h,args,c,fargs)
  | NLF.OMeta(e,_,x,c,fargs) -> NLF.OMeta(e,s,x,c,fargs)
let with_env e = function
  | NLF.Obj(_,s,h,args,c,fargs) -> NLF.Obj(e,s,h,args,c,fargs)
  | NLF.OMeta(_,s,x,c,fargs) -> NLF.OMeta(e,s,x,c,fargs)
let no_env = with_env E.empty

let ohead = function
  | XLFn.HVar x -> NLF.HVar x
  | XLFn.HConst c -> NLF.HConst c

let rec obj term = function
  | XLFn.OLam (x,a,t) ->
    let a = fam (no_env term) a in
    obj (env_add x a term) t
  | XLFn.OMeta (x,XLFn.FConst(c,m)) ->
    let sigma, fargs = args (no_env term) m in
    NLF.OMeta(env_of term, sigma, x, c, fargs)
  | XLFn.OHead (h,l,XLFn.FConst(c,m)) ->
    let sigma, oargs = args (no_env term) l in
    let sigma, fargs = args (with_subst sigma (no_env term)) m in
    NLF.Obj(env_of term, sigma, ohead h, oargs, c, fargs)
  | XLFn.OBox(t,p,s) ->
    let term = with_env (env_of term) (go p term) in
    obj term t          (* TODO subst *)

and arg term (x,t) : S.t * (variable * NLF.obj) = match t with
  | XLFn.OHead(h,l,XLFn.FConst(c,m)) ->
      let sigma, fargs = args (no_env term) m in
      if l = [] then
        sigma, (x, NLF.Obj(E.empty, S.empty, ohead h, A.empty, c, fargs))
      else 
        let z = Name.gen_definition() in
	let sigma, oargs = args (with_subst sigma (no_env term)) l in
	S.add z (ohead h, oargs, c, fargs) sigma,
	(x, NLF.OMeta(E.empty, S.empty, z, c, fargs)) (* TODO: S: include defs de fargs? *)
  | XLFn.OLam _ ->
      subst_of term, (x, obj term t)
  | XLFn.OMeta(z,XLFn.FConst(c,m)) -> 
      let sigma, fargs = args (no_env term) m in
      sigma, (x, NLF.OMeta(E.empty, S.empty, z, c, fargs))
  | XLFn.OBox(t,p,s) ->
    let term = with_env (env_of term) (go p term) in
    arg term (x,t)    (* TODO subst *)

and args term : XLFn.args -> S.t * A.t =  function
  | [] -> subst_of term, A.empty
  | e :: l -> 
      let sigma, (x,t) = arg term e in
      let sigma, l = args (with_subst sigma (no_env term)) l in
      sigma, A.add x t l

and fam term = function
  | XLFn.FProd (x,a,b) ->
    let a = fam (no_env term) a in
    fam (env_add x a term) b
  | XLFn.FHead(XLFn.FConst(c,l)) ->
      let sigma, fargs = args (no_env term) l in
      NLF.Fam(env_of term, sigma, c, fargs)

let rec kind term = function
  | XLFn.KProd(x, a, k) ->
    let a = fam (no_env term) a in
    kind (env_add x a term) k
  | XLFn.KType -> NLF.KType (env_of term)

let entry kont nlfs = function 
    | XLFn.ODecl a -> kont nlfs (NLF.ODecl (fam bidon a))
    | XLFn.FDecl k -> kont nlfs (NLF.FDecl (kind bidon k))

(* and back *)

let s_merge s1 s2 =
  S.fold S.add s1 s2

let from_ohead = function
  | NLF.HVar x -> XLFn.HVar x
  | NLF.HConst c -> XLFn.HConst c

let rec from_obj sigma = function
  | NLF.Obj(env, sigma', h, args, c, fargs) ->
    let sigma = s_merge sigma sigma' in
    E.fold (fun x a t -> XLFn.OLam(x, from_fam sigma a, t)) env
      (XLFn.OHead(from_ohead h, from_args sigma args, XLFn.FConst(c, from_args sigma fargs)))
  | NLF.OMeta(env, sigma', x, c, fargs) as t ->
    let sigma = s_merge sigma sigma' in
    let h, oa, c, fa =
      try S.find x sigma
      with Not_found ->
	Format.printf "%a not found in %a@." Name.Pp.definition x Pp.obj (with_subst sigma t);
	Error.global_error "definition not found" "" in
    E.fold (fun x a t -> XLFn.OLam(x, from_fam sigma a, t)) env
      (XLFn.OHead(from_ohead h, from_args sigma oa, XLFn.FConst(c, from_args sigma fa)))

and from_args sigma args =
  A.fold (fun x t l -> (x, from_obj sigma t) :: l) args []

and from_fam sigma (NLF.Fam(env,sigma',c,fargs)) =
  let sigma = s_merge sigma sigma' in
  let l = from_args sigma fargs in
  E.fold (fun x a b -> XLFn.FProd(x, from_fam sigma a, b)) env (XLFn.FHead(XLFn.FConst(c,l)))

let rec from_kind (NLF.KType env) = 
  E.fold (fun x a k ->  XLFn.KProd(x, from_fam S.empty a, k)) env XLFn.KType

let from_obj = from_obj S.empty
let from_fam = from_fam S.empty

let rec from_sign s = NLFSign.fold
  (fun x e s -> (x, match e with
    | NLF.FDecl k -> XLFn.FDecl (from_kind k)
    | NLF.ODecl a -> XLFn.ODecl (from_fam a)) :: s
  ) s []
