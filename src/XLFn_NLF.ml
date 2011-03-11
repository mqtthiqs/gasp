open Name
open NLF

module E = NLFEnv
module S = NLFSubst
module A = NLFArgs

let ohead = function
  | XLFn.HVar x -> NLF.HVar x
  | XLFn.HConst c -> NLF.HConst c

let rec obj term env = function
  | XLFn.OLam (x,a,t) -> obj term (E.add x (fam term E.empty a) env) t
  | XLFn.OMeta (x,XLFn.FConst(c,m)) ->
      let sigma, fargs = args term S.empty m in
      NLF.OMeta(env, sigma, x, c, fargs)
  | XLFn.OHead (h,l,XLFn.FConst(c,m)) -> 
      let sigma, oargs = args term S.empty l in
      let sigma, fargs = args term sigma m in
      NLF.Obj(env, sigma, ohead h, oargs, c, fargs)
  | XLFn.OBox(t,p,s) -> 
      obj (go p term) env t		(* TODO subst *)

and obj1 term sigma (x,t) : S.t * (variable * NLF.obj) = match t with
  | XLFn.OHead(h,l,XLFn.FConst(c,m)) ->
      let sigma, fargs = args term sigma m in
      if l = [] then
	sigma, (x, NLF.Obj(E.empty, S.empty, ohead h, A.empty, c, fargs))
      else 
	let z = Name.gen_definition() in
	let sigma, oargs = args term sigma l in
	S.add z (ohead h, oargs, c, fargs) sigma,
	(x, NLF.OMeta(E.empty, S.empty, z, c, fargs)) (* TODO: S: include defs de fargs? *)
  | XLFn.OLam _ -> 
      sigma, (x, obj term E.empty t)
  | XLFn.OMeta(z,XLFn.FConst(c,m)) -> 
      let sigma, fargs = args term sigma m in
      sigma, (x, NLF.OMeta(E.empty, S.empty, z, c, fargs))
  | XLFn.OBox _ -> assert false		(* TODO *)

and args term sigma : XLFn.args -> S.t * A.t =  function
  | [] -> sigma, A.empty
  | e :: l -> 
      let sigma, (x,t) = obj1 term sigma e in
      let sigma, l = args term sigma l in
      sigma, A.add x t l

and fam term env = function
  | XLFn.FProd (x,a,b) -> fam term (E.add x (fam term E.empty a) env) b
  | XLFn.FHead(XLFn.FConst(c,l)) ->
      let sigma, fargs = args term S.empty l in
      NLF.Fam(env, sigma, c, fargs)

let rec kind term env = function
  | XLFn.KProd(x, a, k) -> kind term (E.add x (fam term E.empty a) env) k
  | XLFn.KType -> NLF.KType env

let entry kont nlfs = function 
    | XLFn.ODecl a -> kont nlfs (NLF.ODecl (fam bidon E.empty a))
    | XLFn.FDecl k -> kont nlfs (NLF.FDecl (kind bidon E.empty k))

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
      let h, oa, c, fa = 
	try S.find x sigma 
	with Not_found -> 
	  Format.printf "%a not found in %a@." Name.Pp.definition x Pp.obj t;
	  Error.global_error "definition not found" "" in
      XLFn.OHead(from_ohead h, from_args sigma oa, XLFn.FConst(c, from_args sigma fa))

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
