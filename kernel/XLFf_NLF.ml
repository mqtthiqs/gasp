open XLFf
open NLF

module E = Name.Varmap

let rec obj sign env repo : XLFf.obj -> NLF.obj = function
  | XLFf.OBox (t,p,u) ->
    let d = Obj.magic (obj sign env repo u) in (* TODO *)
    let repo = go repo p d in
    obj sign env repo t
  | XLFf.Obj (sigma, v) -> assert false	(* TODO *)

and arg sign env repo a : XLFf.value -> NLF.subst * NLF.value = function
  | XLFf.VLam (x,t) ->
    let b = fam sign env repo b in
    NLFSubst.empty, NLF.VLam (x, b, obj sign (E.add x b env) repo t)
  | XLFf.VHead (XLF.HConst c) ->
    match NLFSign.find c sign with
      | NLF.FDecl k -> assert false
      | NLF.ODecl (NLF.FProd _) -> failwith (Name.of_constant c ^ " is not in η-long form")
      | NLF.ODecl (NLF.FHead (sigma, d, l)) ->
	(* TODO verif a *)
	sigma, NLF.VHead (XLF.HConst c, d, l)

and fam sign (env:NLF.fam E.t) repo : XLFf.fam -> NLF.fam = function
  | XLFf.FProd (x, a, b) ->
    let a = fam sign env repo a in
    NLF.FProd (x, a, fam sign (E.add x a env) repo b)
  | XLFf.FHead (sigma, c, m) ->
    assert false			(* TODO *)

let rec kind sign (env: NLF.fam E.t) repo : XLFf.kind -> NLF.kind = function
  | XLFf.KType -> NLF.KType
  | XLFf.KProd (x,a,k) ->
    let a = fam sign env repo a in
    NLF.KProd (x, a, kind sign (E.add x a env) repo k)
