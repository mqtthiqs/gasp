open NLF

module E = NLFEnv
module S = NLFSubst
module A = NLFArgs

let ohead = function
  | XLFn.HVar x -> NLF.HVar x
  | XLFn.HMeta x -> NLF.HDef x
  | XLFn.HConst c -> NLF.HConst c

let rec obj env = function
  | XLFn.OLam (x,a,t) -> obj (E.add x (fam E.empty a) env) t
  | XLFn.OHead (h,l,XLFn.FConst(c,m)) -> 
      let sigma, oargs = args S.empty l in
      let sigma, fargs = args sigma m in
      NLF.Obj(env, sigma, ohead h, oargs, c, fargs)

and obj1 sigma (x,t) : S.t * _ = match t with
  | XLFn.OHead(h,l,XLFn.FConst(c,m)) ->
      let sigma, fargs = args sigma m in
      if l = [] then
	sigma, (x, NLF.Obj(E.empty, sigma, ohead h, A.empty, c, fargs))
      else 
	let z = Name.gen_definition() in
	let sigma, oargs = args sigma l in
	S.add z (ohead h, oargs, c, fargs) sigma,
	(x, NLF.Obj(E.empty, S.empty, NLF.HDef z, A.empty, c, fargs))
  | XLFn.OLam _ -> 
      sigma, (x, obj E.empty t)

and args sigma : XLFn.args -> S.t * A.t =  function
  | [] -> sigma, A.empty
  | e :: l -> 
      let sigma, (x,t) = obj1 sigma e in
      let sigma, l = args sigma l in
      sigma, A.add x t l

and fam env = function
  | XLFn.FProd (x,a,b) -> fam (E.add x (fam E.empty a) env) b
  | XLFn.FHead(XLFn.FConst(c,l)) ->
      let sigma, fargs = args S.empty l in
      NLF.Fam(env, sigma, c, fargs)

let rec kind env = function
  | XLFn.KProd(x, a, k) -> kind (E.add x (fam E.empty a ) env) k
  | XLFn.KType -> NLF.KType env

let entry kont nlfs = function 
    | XLFn.ODecl a -> kont nlfs (NLF.ODecl (fam E.empty a))
    | XLFn.FDecl k -> kont nlfs (NLF.FDecl (kind E.empty k))

(* and back *)

let from_ohead = function
  | NLF.HDef x -> assert false
  | NLF.HVar x -> XLFn.HVar x
  | NLF.HConst c -> XLFn.HConst c

let rec from_obj (NLF.Obj(env,sigma,h,oa,c,fa)) =
  E.fold (fun x a t -> XLFn.OLam(x, from_fam a, t)) env
    begin match h with
      | NLF.HDef x -> 
	  assert (A.is_empty oa);
	  let h, oa, c, fa = S.find x sigma in
	  XLFn.OHead(from_ohead h, from_args sigma oa, XLFn.FConst(c, from_args sigma fa))
      | NLF.HVar x ->
	  XLFn.OHead(XLFn.HVar x, from_args sigma oa, XLFn.FConst(c,from_args sigma fa))
      | NLF.HConst c -> 
	  XLFn.OHead(XLFn.HConst c, from_args sigma oa, XLFn.FConst(c,from_args sigma fa))
    end

and from_args sigma args =
  A.fold (fun x t l -> (x, from_obj t) :: l) args []

and from_fam (NLF.Fam(env,sigma,c,fargs)) =
  let l = from_args sigma fargs in
  E.fold (fun x a b -> XLFn.FProd(x, from_fam a, b)) env (XLFn.FHead(XLFn.FConst(c,l)))

let rec from_kind (NLF.KType env) = 
  E.fold (fun x a k ->  XLFn.KProd(x, from_fam a, k)) env XLFn.KType

let rec from_sign s = NLFSign.fold
  (fun x e s -> (x, match e with
     | NLF.FDecl k -> XLFn.FDecl (from_kind k)
     | NLF.ODecl a -> XLFn.ODecl (from_fam a)) :: s
  ) s []
