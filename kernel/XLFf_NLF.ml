open Name
open XLFf
open NLF

module E = Name.Varmap
module S = NLFSubst

type eent =
  | EDecl of NLF.fam
  | EDef of NLF.def

let rec hsubst_fam x v a b : NLF.fam = assert false
let rec hsubst_kind x v a b : NLF.kind = assert false

let rec ohead sign env repo : ohead -> NLF.fam = function
  | XLF.HConst c -> NLFSign.ODecl.find c (snd sign)
  | XLF.HVar x ->
    try match E.find x env with
      | EDecl a -> a
      | EDef (NLF.DHead(h,a)) -> a
      | EDef (NLF.DApp(_,_,c,m)) -> NLF.FHead(S.empty, c, m)
    with Not_found ->
      try lift_def x repo
      with Not_found -> failwith ("not_found "^Name.of_variable x)

let rec fam sign env repo : fam -> NLF.fam = function
  | FProd(x, a, b) ->
    let a = fam sign env repo a in
    let b = fam sign (E.add x (EDecl a) env) repo b in
    NLF.FProd(x, a, b)

  | FHead (sigma, c, m) ->
    let k = NLFSign.FDecl.find c (fst sign) in
    let env, sigma = subst sign env repo sigma in
    let m = fargs sign env repo sigma (m, k) in
    NLF.FHead (sigma, c, m)

and subst sign env repo : subst -> eent E.t * NLF.def S.t =
  List.fold_left
    begin fun (env, sigma) (x, (h,l)) ->
      let a = ohead sign env repo h in
      let l, (c,m) = args sign env repo sigma (l,a) in
      E.add x (EDef(NLF.DApp(h,l,c,m))) env, S.add x (NLF.DApp(h,l,c,m)) sigma
    end (env, S.empty)

and args sign env repo sigma : args * NLF.fam -> NLF.args * (fconst * NLF.args) = function
  | v :: l, NLF.FProd (x, a, b) ->
    let v = value sign env repo sigma (v, a) in
    let l, (c, m) = args sign env repo sigma (l, hsubst_fam x v a b) in
    v :: l, (c, m)
  | [], NLF.FHead (sigma',h,l) ->
    [], (h,l)
  | _ -> failwith ("args: not applicable")

and fargs sign env repo sigma : args * NLF.kind -> NLF.args = function
  | v :: l, NLF.KProd (x, a, k) ->
    let v = value sign env repo sigma (v, a) in
    v :: fargs sign env repo sigma (l, hsubst_kind x v a k)
  | [], NLF.KType -> []
  | _ -> failwith ("fargs: not applicable")

and obj sign env repo : obj * NLF.fam -> NLF.obj = function
  | Obj(sigma, v), a ->
    let env, sigma = subst sign env repo sigma in
    let v = value sign env repo S.empty (v, a) in
    NLF.Obj (sigma, v)
  | OBox(t,p,u), a -> assert false

and value sign env repo sigma : value * NLF.fam -> NLF.value = function
  | VLam (x,t), NLF.FProd (y, a, b) ->
    assert (x=y);
    let t = obj sign (E.add x (EDecl a) env) repo (t, b) in
    NLF.VLam (x, a, t)

  | VHead h, NLF.FHead (sigma, c, m) ->
    begin match ohead sign env repo h with
      | NLF.FProd _ -> failwith ("Fct au lieu de valeur")
      | NLF.FHead (sigma', c', m') -> assert false
    end
  | VLam _, NLF.FHead _ -> failwith ("Lam attend prod")
  | VHead _, NLF.FProd _ -> failwith ("Pas en forme eta-longue")

let rec kind sign env repo : kind -> NLF.kind = function
  | KProd(x, a, k) ->
    let a = fam sign env repo a in
    let k = kind sign (E.add x (EDecl a) env) repo k in
    NLF.KProd(x, a, k)
  | KType -> NLF.KType


let obj sign repo = obj sign E.empty repo
let fam sign repo = fam sign E.empty repo
let kind sign repo = kind sign E.empty repo
