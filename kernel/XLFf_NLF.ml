open Name
open XLFf
open NLF

module E = Name.Varmap
module S = NLFSubst

type eent =
  | EDecl of NLF.fam
  | EDef of NLF.def

let rec hsubst_fam x v a b : NLF.fam = b (* TODO *)
let rec hsubst_kind x v a k : NLF.kind = k (* TODO *)

let rec equals_args sign repo env (sigma, m) (sigma', m') = match m, m' with
  | v :: m, v' :: m' ->
    equals_val sign repo env (sigma, v) (sigma', v');
    equals_args sign repo env (sigma, m) (sigma', m')
  | [], [] -> ()
  | _ -> failwith ("Not convertible")

and equals_val sign repo env (sigma, v) (sigma', v') = match v, v' with
  | NLF.VHead (h,_,_), NLF.VHead (h',_,_) ->
    (* assert (h=h') 			(\* TODO *\) *)
    ()
  | NLF.VLam (x, _, t), NLF.VLam (x', _, t') ->
    (* assert (x=x');			(\* TODO *\) *)
    (* assert (t=t') *)
    ()
  | _ -> failwith ("Not convertible")

let rec ohead sign repo env : ohead -> NLF.fam = function
  | XLF.HConst c -> NLFSign.ODecl.find c (snd sign)
  | XLF.HVar x ->
    try match E.find x env with
      | EDecl a -> a
      | EDef (NLF.DHead(h,a)) -> a
      | EDef (NLF.DApp(_,_,c,m)) -> NLF.FHead(S.empty, c, m)
    with Not_found ->
      try lift_def x repo
      with Not_found -> failwith ("not_found "^Name.of_variable x)

let rec fam sign repo env : fam -> NLF.fam = function
  | FProd(x, a, b) ->
    let a = fam sign repo env a in
    let b = fam sign repo (E.add x (EDecl a) env) b in
    NLF.FProd(x, a, b)

  | FHead (sigma, c, m) ->
    let k = NLFSign.FDecl.find c (fst sign) in
    let env, sigma = subst sign repo env sigma in
    let m = fargs sign repo env (m, k) in
    NLF.FHead (sigma, c, m)

and subst sign repo env : subst -> eent E.t * NLF.def S.t =
  List.fold_left
    begin fun (env, sigma) (x, (h,l)) ->
      let a = ohead sign repo env h in
      let l, (c,m) = args sign repo env (l,a) in
      E.add x (EDef(NLF.DApp(h,l,c,m))) env, S.add x (NLF.DApp(h,l,c,m)) sigma
    end (env, S.empty)

and args sign repo env : args * NLF.fam -> NLF.args * (fconst * NLF.args) = function
  | v :: l, NLF.FProd (x, a, b) ->
    let v = value sign repo env (v, a) in
    let l, (c, m) = args sign repo env (l, hsubst_fam x v a b) in
    v :: l, (c, m)
  | [], NLF.FHead (sigma',h,l) ->
    [], (h,l)
  | _ -> failwith ("args: not applicable")

and fargs sign repo env : args * NLF.kind -> NLF.args = function
  | v :: l, NLF.KProd (x, a, k) ->
    let v = value sign repo env (v, a) in
    v :: fargs sign repo env (l, hsubst_kind x v a k)
  | [], NLF.KType -> []
  | _ -> failwith ("fargs: not applicable")

and obj sign repo env : obj * NLF.fam -> NLF.obj = function
  | Obj(sigma, v), a ->
    let env, sigma = subst sign repo env sigma in
    let v = value sign repo env (v, a) in
    NLF.Obj (sigma, v)
  | OBox(t,p,u), a -> assert false

and value sign repo env : value * NLF.fam -> NLF.value = function
  | VLam (x,t), NLF.FProd (y, a, b) ->
    (* assert (x=y); *)
    let t = obj sign repo (E.add x (EDecl a) env) (t, b) in
    NLF.VLam (x, a, t)

  | VHead h, NLF.FHead (sigma, c, m) ->
    begin match ohead sign repo env h with
      | NLF.FProd _ -> failwith ("Fct au lieu de valeur")
      | NLF.FHead (sigma', c', m') ->
	if c <> c' then failwith ("Not convertible: "^Name.of_fconst c^" <-> "^Name.of_fconst c');
	equals_args sign repo env (sigma, m) (sigma', m');
	NLF.VHead (h, c, m)
    end
  | VLam _, NLF.FHead _ -> failwith ("Lam attend prod")
  | VHead _, NLF.FProd _ -> failwith ("Pas en forme eta-longue")

let rec kind sign repo env : kind -> NLF.kind = function
  | KProd(x, a, k) ->
    let a = fam sign repo env a in
    let k = kind sign repo (E.add x (EDecl a) env) k in
    NLF.KProd(x, a, k)
  | KType -> NLF.KType

let obj sign repo = obj sign repo E.empty
let fam sign repo = fam sign repo E.empty
let kind sign repo = kind sign repo E.empty
