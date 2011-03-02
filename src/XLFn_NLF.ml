open NLF

module E = NLFEnv
module S = NLFSubst

let ohead = function
  | XLFn.HVar x | XLFn.HMeta x -> NLF.HVar x
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
	sigma, (x, NLF.Obj(E.empty, sigma, ohead h, S.empty, c, fargs))
      else 
	let z = Name.gen_name() in
	let sigma, oargs = args sigma l in
	S.add z (NLF.Obj(E.empty, S.empty, NLF.HVar x, oargs, c, fargs)) sigma,
	(x, NLF.Obj(E.empty, S.empty, NLF.HVar z, S.empty, c, fargs))
  | XLFn.OLam _ -> 
      sigma, (x, obj E.empty t)

and args sigma : XLFn.args -> S.t * S.t =  function
  | [] -> sigma, S.empty
  | e :: l -> 
      let sigma, (x,t) = obj1 sigma e in
      let sigma, l = args sigma l in
      sigma, S.add x t l

and fam env = function
  | XLFn.FProd (x,a,b) -> fam (E.add x (fam E.empty a) env) b
  | XLFn.FHead(XLFn.FConst(c,l)) ->
      let sigma, fargs = args S.empty l in
      NLF.Fam(env, sigma, c, fargs)

let rec kind env = function
  | XLFn.KProd(x, a, k) -> kind (E.add x (fam E.empty a ) env) k
  | XLFn.KType -> NLF.KType env

let entry kont nlfs = function 
    | XLFn.ODecl a -> kont nlfs (NLFSign.ODecl (fam E.empty a))
    | XLFn.FDecl k -> kont nlfs (NLFSign.FDecl (kind E.empty k))

(* and back *)

let rec from_obj t = assert false

and from_args sigma args =
  S.fold 
    (fun x t l -> 
       l
    ) args []

and from_fam (NLF.Fam(env,sigma,c,fargs)) =
  let l = from_args sigma fargs in
  E.fold (fun x a acc -> XLFn.FProd(x, from_fam a, acc)) env (XLFn.FHead(c,l))
  
let rec from_kind (NLF.KType env) = 
  E.fold (fun x a acc ->  XLFn.KProd(x, from_fam a, acc)) env XLFn.KType

let rec from_sign s = assert false
