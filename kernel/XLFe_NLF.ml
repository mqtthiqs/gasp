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

let rec obj term = function
  | XLFe.OLam (x,a,t) ->
    let a = fam (no_env term) a in
    obj (env_add x a term) t
  | XLFe.OMeta (x,XLFe.FConst(c,m)) ->
    let sigma, fargs = args (no_env term) m in
    NLF.OMeta(env_of term, sigma, x, c, fargs)
  | XLFe.OHead (h,l,XLFe.FConst(c,m)) ->
    let sigma, oargs = args (no_env term) l in
    let sigma, fargs = args (with_subst sigma (no_env term)) m in
    NLF.Obj(env_of term, sigma, h, oargs, c, fargs)
  | XLFe.OBox(t,p,s) ->
    let term = with_env (env_of term) (go p term) in
    obj term t          (* TODO subst *)

and arg term (x,t) : S.t * (variable * NLF.obj) = match t with
  | XLFe.OHead(h,l,XLFe.FConst(c,m)) ->
      let sigma, fargs = args (no_env term) m in
      if l = [] then
        sigma, (x, NLF.Obj(E.empty, S.empty, h, A.empty, c, fargs))
      else 
        let z = Name.gen_definition() in
	let sigma, oargs = args (with_subst sigma (no_env term)) l in
	S.add z (h, oargs, c, fargs) sigma,
	(x, NLF.OMeta(E.empty, S.empty, z, c, fargs)) (* TODO: S: include defs de fargs? *)
  | XLFe.OLam _ ->
      subst_of term, (x, obj term t)
  | XLFe.OMeta(z,XLFe.FConst(c,m)) -> 
      let sigma, fargs = args (no_env term) m in
      sigma, (x, NLF.OMeta(E.empty, S.empty, z, c, fargs))
  | XLFe.OBox(t,p,s) ->
    let term = with_env (env_of term) (go p term) in
    arg term (x,t)    (* TODO subst *)

and args term : XLFe.args -> S.t * A.t =  function
  | [] -> subst_of term, A.empty
  | e :: l -> 
      let sigma, (x,t) = arg term e in
      let sigma, l = args (with_subst sigma (no_env term)) l in
      sigma, A.add x t l

and fam term = function
  | XLFe.FProd (x,a,b) ->
    let a = fam (no_env term) a in
    fam (env_add x a term) b
  | XLFe.FHead(XLFe.FConst(c,l)) ->
      let sigma, fargs = args (no_env term) l in
      NLF.Fam(env_of term, sigma, c, fargs)

let rec kind term = function
  | XLFe.KProd(x, a, k) ->
    let a = fam (no_env term) a in
    kind (env_add x a term) k
  | XLFe.KType -> NLF.KType (env_of term)

let entry kont nlfs = function 
    | XLFe.ODecl a -> kont nlfs (NLF.ODecl (fam bidon a))
    | XLFe.FDecl k -> kont nlfs (NLF.FDecl (kind bidon k))

(* and back *)

let s_merge s1 s2 =
  S.fold S.add s1 s2

let rec from_obj sigma = function
  | NLF.Obj(env, sigma', h, args, c, fargs) ->
    let sigma = s_merge sigma sigma' in
    E.fold (fun x a t -> XLFe.OLam(x, from_fam sigma a, t)) env
      (XLFe.OHead(h, from_args sigma args, XLFe.FConst(c, from_args sigma fargs)))
  | NLF.OMeta(env, sigma', x, c, fargs) as t ->
    let sigma = s_merge sigma sigma' in
    let h, oa, c, fa =
      try S.find x sigma
      with Not_found ->
	Format.printf "%a not found in %a@." Name.Pp.definition x Pp.obj (with_subst sigma t);
	Error.global_error "definition not found" "" in
    E.fold (fun x a t -> XLFe.OLam(x, from_fam sigma a, t)) env
      (XLFe.OHead(h, from_args sigma oa, XLFe.FConst(c, from_args sigma fa)))

and from_args sigma args =
  A.fold (fun x t l -> (x, from_obj sigma t) :: l) args []

and from_fam sigma (NLF.Fam(env,sigma',c,fargs)) =
  let sigma = s_merge sigma sigma' in
  let l = from_args sigma fargs in
  E.fold (fun x a b -> XLFe.FProd(x, from_fam sigma a, b)) env (XLFe.FHead(XLFe.FConst(c,l)))

let rec from_kind (NLF.KType env) = 
  E.fold (fun x a k ->  XLFe.KProd(x, from_fam S.empty a, k)) env XLFe.KType

let from_obj = from_obj S.empty
let from_fam = from_fam S.empty

let rec from_sign s = NLFSign.fold
  (fun x e s -> (x, match e with
    | NLF.FDecl k -> XLFe.FDecl (from_kind k)
    | NLF.ODecl a -> XLFe.ODecl (from_fam a)) :: s
  ) s []
