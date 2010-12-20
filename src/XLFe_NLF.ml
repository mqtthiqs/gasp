open Name
open NLF

module CMap' = Map.Make(struct type t = variable let compare = Pervasives.compare end)
module CMap = struct
  type 'a t = 'a CMap'.t
  let add x a m = CMap'.add x a m
  let find x m = try CMap'.find x m with Not_found -> failwith (x^" not found")
  let empty = CMap'.empty
end

type names = variable list
type arity = names CMap.t

(* From XLFe to NLF: environment building and forgetting arguments
   positions *)

let rec obj arity env names : XLFe.obj -> NLF.obj * names = function
  | XLFe.OLam (x,a,t) -> 
      let (a, ns) = fam arity NLFEnv.empty [] a in
      obj
	(CMap.add x ns arity)
	(NLFEnv.add env x (NLFEnv.ODecl a)) 
	(x::names) t
  | XLFe.OHead h -> 
      let (ht, ha) = ohead arity env h in
      NLF.Obj(env, ht, ha), names

and args arity names l = 
  List.fold_left2
    ( fun env t x ->
	let (t, ns) = obj arity NLFEnv.empty [] t in
	NLFEnv.add env x (NLFEnv.ODef t)
    ) NLFEnv.empty l names

and ohead arity env : XLFe.ohead -> NLF.ohead * NLF.fhead = function
  | XLFe.OVar(x,l,a) -> 
      NLF.OVar(x, args arity (CMap.find x arity) l), fhead arity env a
  | XLFe.OConst(c,l,a) -> 
      NLF.OConst(c, args arity (CMap.find c arity) l), fhead arity env a
  | XLFe.OApp(t,l,a) -> 
      let (t, ns) = obj arity NLFEnv.empty [] t in
      NLF.OApp(t, args arity ns l), fhead arity env a

and fam arity env names : XLFe.fam -> NLF.fam * names = function
  | XLFe.FProd (x,a,b) -> 
      let (a, ns) = fam arity NLFEnv.empty [] a in
      fam 
	(CMap.add x ns arity)
	(NLFEnv.add env x (NLFEnv.ODecl a))
	(x::names) b
  | XLFe.FHead h -> 
      NLF.Fam(env, fhead arity env h), names

and fhead arity env : XLFe.fhead -> NLF.fhead = function
  | XLFe.FConst(c,l,XLFe.KType) -> NLF.FConst(c, args arity (CMap.find c arity) l)

let rec kind arity env names : XLFe.kind -> NLF.kind * names = function
  | XLFe.KProd(x,a,k) -> 
      let (a, ns) = fam arity NLFEnv.empty [] a in
      kind 
	(CMap.add x ns arity)
	(NLFEnv.add env x (NLFEnv.ODecl a))
	(x::names) k
  | XLFe.KHead(XLFe.KType) -> 
      NLF.Kind env, names

let rec sign arity s : XLFe.sign -> NLFSign.t = function
  | [] -> s
  | (c, XLFe.FDecl k) :: tl ->
      let (k, ns) = kind arity NLFEnv.empty [] k in
      sign 
	(CMap.add c ns arity)
	(NLFSign.add s c (NLFSign.FDecl k))
	tl
  | (c, XLFe.ODecl a) :: tl ->
      let (a, ns) = fam arity NLFEnv.empty [] a in
      sign 
	(CMap.add c ns arity)
	(NLFSign.add s c (NLFSign.ODecl a))
	tl

(* ...and back *)

