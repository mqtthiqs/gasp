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
	S.add z (NLF.Obj(E.empty, S.empty, x, oargs, c, fargs )) sigma,
      (x, NLF.Obj(E.empty, S.empty, NLF.HVar z, S.empty, c, fargs))
  | XLFn.OLam _ -> 
      sigma, (x, obj E.empty t)

and args sigma : XLFn.args -> S.t * S.t =  function
  | [] -> sigma, S.empty
  | e :: l -> 
      let (sigma, (x,t)) = obj1 sigma e in
      let (sigma, l) = args sigma l in
      sigma, S.add x t l

and fam env = function
  | XLFn.FProd (x,a,b) -> fam (E.add x (fam E.empty a) env) b
  | XLFn.FHead(XLFn.FConst(c,l)) -> assert false

let entry kont nlfs = function 
    | XLFn.ODecl a -> kont nlfs (NLFSign.ODecl (fam a))
    | XLFn.FDecl k -> kont nlfs (NLFSign.FDecl (kind k))

(* and back *)

let rec from_obj t = assert false
and from_fam a = assert false
let rec from_kind k = assert false
let rec from_sign s = assert false
