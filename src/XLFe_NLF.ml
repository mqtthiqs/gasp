open Name
open NLF

(* From XLFe to NLF: environment building and forgetting arguments
   positions *)

let rec obj env = function
  | XLFe.OLam (x,a,t) -> 
      let a = fam NLFEnv.empty a in
      obj (NLFEnv.add env x (NLFEnv.ODecl a)) t
  | XLFe.OHead h -> 
      let (ht, ha) = ohead env h in
      NLF.Obj(env, ht, ha)

and args l = 
  List.fold_left
    ( fun env (x,t) ->
	let t = obj NLFEnv.empty t in
	NLFEnv.add env x (NLFEnv.ODef t)
    ) NLFEnv.empty l

and ohead env = function
  | XLFe.OVar(x,l,a) -> NLF.OVar(x, args l), fhead env a
  | XLFe.OConst(c,l,a) -> NLF.OConst(c, args l), fhead env a
  | XLFe.OApp(t,l,a) -> 
      let t = obj NLFEnv.empty t in
      NLF.OApp(t, args l), fhead env a

and fam env = function
  | XLFe.FProd (x,a,b) -> 
      let a = fam NLFEnv.empty a in
      fam (NLFEnv.add env x (NLFEnv.ODecl a)) b
  | XLFe.FHead h -> 
      NLF.Fam(env, fhead env h)

and fhead env = function
  | XLFe.FConst(c,l,XLFe.KType) -> NLF.FConst(c, args l)

let rec kind env = function
  | XLFe.KProd(x,a,k) -> 
      let a = fam NLFEnv.empty a in
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

