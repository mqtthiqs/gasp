open XLFf
open NLF

module E = Name.Varmap
module S = NLFSubst

let subst_of (NLF.Obj(sigma, _)) = sigma
let add_subst x d (NLF.Obj(sigma, v)) = NLF.Obj (S.add x d sigma, v)

let rec ohead sign env repo : XLFf.ohead -> NLF.fam = function
  | XLF.HConst c -> NLFSign.ODecl.find c (snd sign)
  | XLF.HVar x ->
    try E.find x env
    with Not_found ->
      try lift_def x repo
      with Not_found -> failwith ("not_found "^Name.of_variable x)

let rec obj sign env repo : XLFf.obj -> NLF.obj = function
  | XLFf.OBox (t,p,u) ->
    let d = assert false in (* TODO *)
    let repo = go repo p d in
    obj sign env repo t
  | XLFf.Obj (sigma, v) ->
    let repo = subst sign env repo sigma in

and arg sign env repo : XLFf.value * NLF.fam -> NLF.subst * NLF.value = function
  | XLFf.VLam (x,t), NLF.FProd (y,a,b) ->
    assert (x=y);
    NLFSubst.empty, NLF.VLam (x, b, obj sign (E.add x b env) repo t)
  | XLFf.VHead h, NLF.FHead (sigma, c, m) -> (* TODO *)
    begin match ohead sign env repo h with
      | NLF.FProd _ -> failwith ("not eta-long normal form")
      | NLF.FHead(sigma, c, m) ->
	(* TODO verif a *)
	sigma, NLF.VHead (h, c, m)
    end
  | _ -> failwith ("not eta-long 2")

and oargs sign env repo l : XLFf.args * NLF.fam -> NLF.args * Name.fconst * NLF.args = function
  | v :: m, NLF.FProd (x, a, b) ->
    let sigma, v = arg sign env repo (v, a) in
    oargs sign env repo (v :: l) (m, a)
  | [], NLF.FHead (sigma, c, m) ->
    l, c, m
  | _ -> failwith "oargs"

and fargs sign env repo : XLFf.args * NLF.kind -> NLF.args = function
  | v :: m, NLF.KProd (x, a, k) ->
    let sigma, v = arg sign env repo (v, a) in
    v :: fargs sign env repo (m, k)	(* TODO sigma? *)
  | [], NLF.KType -> []
  | _ -> failwith "fargs"

and fam sign env repo : XLFf.fam -> NLF.obj * NLF.fam = function
  | XLFf.FProd (x, a, b) ->
    let repo, a = fam sign env repo a in
    let repo, b = fam sign (E.add x a env) repo b in
    repo, NLF.FProd (x, a, b)
  | XLFf.FHead (sigma, c, m) ->
    let repo = subst sign env repo sigma in
    let k = NLFSign.FDecl.find c (fst sign) in
    let m = fargs sign env repo (m, k) in
    repo, NLF.FHead (subst_of repo, c, m)

and subst sign env repo : XLFf.subst -> NLF.obj =
  List.fold_left begin
    fun repo (x, (h, l)) ->
      let a = ohead sign env repo h in
      let l, c, m = oargs sign env repo [] (l, a) in
      add_subst x (NLF.DApp(h, l, c, m)) repo
  end repo

let rec kind sign (env: NLF.fam E.t) repo : XLFf.kind -> NLF.obj * NLF.kind = function
  | XLFf.KType -> repo, NLF.KType
  | XLFf.KProd (x,a,k) ->
    let repo, a = fam sign env repo a in
    let repo, k = kind sign (E.add x a env) repo k in
    repo, NLF.KProd (x, a, k)

let obj sign repo = obj sign E.empty repo
let fam sign repo = fam sign E.empty repo
let kind sign repo = kind sign E.empty repo
