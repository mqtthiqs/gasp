open Name
open NLF

module E = Name.Varmap
module S = Name.Varmap

module NLF_Utils = struct

  let lift_def x = function
    | Obj(subst, _) ->
      match Varmap.find x subst with
	| DAtom (_, _, fa) -> FAtom fa
	| DHead (_, a) -> a

  let go term p = match term, p with
    | Obj(_, t), None -> t                (* TODO que faire de _? *)
    | Obj(s, _), Some (x, n) ->
      try match Varmap.find x s with
	| DHead (h, a) -> failwith "position is not an application"
	| DAtom (h, l, _) -> List.nth l n
      with Not_found -> failwith ("go: variable not found "^(of_variable x))

  let bind x d = function
    | Obj (s, v) -> Obj (Varmap.add x d s, v)

end

let rec equals_fatom sign repo env = function
  | (s, c, m), (s', c', m') ->
    if c <> c'				(* TODO s <> s'? *)
    then failwith ("Not convertible: "^Name.of_fconst c^" <-> "^Name.of_fconst c')
    else equals_args sign repo env (m, m')

and equals_args sign repo env : _ -> unit = function
  | t :: m, t' :: m' ->
    equals_value sign repo env (t, t');
    equals_args sign repo env (m, m')
  | [], [] -> ()
  | _ -> failwith ("Not convertible 1")

and equals_value sign repo env = function
  | _ ->
    ()				(* TODO *)

let rec ohead sign repo env : XLFf.ohead -> NLF.fam = function
  | XLF.HConst c -> Oconstmap.find c (snd sign)
  | XLF.HVar x ->
    try E.find x env
    with Not_found ->
      try NLF_Utils.lift_def x repo
      with Not_found -> failwith ("not_found "^Name.of_variable x)

and subst sign (NLF.Obj(sigma, _) as repo) env : XLFf.subst -> NLF.subst =
  List.fold_left
    begin fun sigma (x, (h,l)) ->
      let a = ohead sign repo env h in
      let l, p = args sign repo env (l, a) in
      S.add x (NLF.DAtom(h,l,p)) sigma
    end sigma

and args sign repo env : XLFf.args * NLF.fam -> NLF.args * NLF.fatom = function
  | v :: l, NLF.FProd (x, a, b) ->
    let v = value sign repo env (v, a) in
    let l, p = args sign repo env (l, b) in (* TODO subst (x/a) b *)
    v :: l, p
  | [], NLF.FAtom p -> [], p
  | _ -> failwith ("args: not applicable")

and fargs sign repo env : XLFf.args * NLF.kind -> NLF.args = function
  | v :: l, NLF.KProd (x, a, k) ->
    let v = value sign repo env (v, a) in
    let l = fargs sign repo env (l, k) in (* TODO subst (x/a) k *)
    v :: l
  | [], NLF.KType -> []
  | _ -> failwith ("args: not applicable")

and obj sign repo env : XLFf.obj * NLF.fam -> NLF.obj = function
  | XLFf.Obj(sigma, v), a ->
    let sigma = subst sign repo env sigma in
    let v = value sign repo env (v, a) in
    NLF.Obj (sigma, v)
  | XLFf.OBox(t,p,u), a ->
    match NLF_Utils.go repo p with
      | NLF.VLam (x, b, repo) ->
	let d = match obj sign repo env (u, b) with
	  | NLF.Obj (sigma, NLF.VHead (h, p)) -> NLF.DHead(h, NLF.FAtom p) (* TODO sigma *)
	  | NLF.Obj (sigma, NLF.VLam _) -> failwith "No lambdas in box argument" in
	obj sign (NLF_Utils.bind x d repo) env (t, a)
      | _ -> failwith "Position is not a lambda"

and value sign repo env : XLFf.value * NLF.fam -> NLF.value = function
  | XLFf.VLam (x,t), NLF.FProd (y, a, b) -> (* TODO x and y *)
    (* assert (x=y); *)
    let t = obj sign repo (E.add x a env) (t, b) in
    NLF.VLam (x, a, t)

  | XLFf.VHead h, NLF.FAtom p ->
    begin match ohead sign repo env h with
      | NLF.FProd _ -> failwith ("Fct au lieu de valeur")
      | NLF.FAtom p' ->
	equals_fatom sign repo env (p, p');
	NLF.VHead (h, p)		(* or p' *)
    end
  | XLFf.VLam _, NLF.FAtom _ -> failwith ("Lam attend prod")
  | XLFf.VHead _, NLF.FProd _ -> failwith ("Pas en forme eta-longue")

let rec fam sign repo env = function
  | XLFf.FAtom (s, c, l) ->
    let s = subst sign repo env s in
    let k = Fconstmap.find c (fst sign) in
    let l = fargs sign repo env (l, k) in
    NLF.FAtom (s, c, l)
  | XLFf.FProd (x, a, b) ->
    let a = fam sign repo env a in
    let b = fam sign repo (E.add x a env) b in
    NLF.FProd (x, a, b)

let rec kind sign repo env = function
  | XLFf.KProd (x, a, k) ->
    let a = fam sign repo env a in
    let k = kind sign repo (E.add x a env) k in
    NLF.KProd (x, a, k)
  | XLFf.KType -> NLF.KType

let obj sign repo (t, a) = obj sign repo E.empty (t, a)
let fam sign repo a = fam sign repo E.empty a
let kind sign repo k = kind sign repo E.empty k
