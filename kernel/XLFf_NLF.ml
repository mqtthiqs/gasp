open Name

module E = Name.Varmap
module S = Name.Varmap

module NLF_Utils = struct

  let lift_def x = function
    | NLF.Obj(subst, _) ->
      match Varmap.find x subst with
	| NLF.DAtom (_, _, fa) -> NLF.FAtom fa
	| NLF.DHead (_, a) -> a

  let go term p = match term, p with
    | NLF.Obj(_, t), None -> t                (* TODO que faire de _? *)
    | NLF.Obj(s, _), Some (x, n) ->
      try match Varmap.find x s with
	| NLF.DHead (h, a) -> failwith "position is not an application"
	| NLF.DAtom (h, l, _) -> List.nth l n
      with Not_found -> failwith ("go: variable not found "^(of_variable x))

  let bind x d = function
    | NLF.Obj (s, v) -> NLF.Obj (Varmap.add x d s, v)

end

module Refresh = struct

  let head y z = function
    | Cst c -> Cst c
    | Var x when x=y -> Var z
    | Var x -> Var x

  let rec fam y z = function
    | NLF.FProd (x, a, b) when x=y -> NLF.FProd (x, fam y z a, b)
    | NLF.FProd (x, a, b) -> NLF.FProd (x, fam y z a, fam y z b)
    | NLF.FAtom p -> NLF.FAtom (fatom y z p)

  and fatom y z (s, c, l) =
    if Varmap.mem y s then (s, c, l) else (* TODO est-ce nécessaire puisque y et Vars(z) st disjoints?? *)
      subst y z s, c, List.map (value y z) l

  and value y z = function
    | NLF.VLam (x, a, t) when x=y -> NLF.VLam (x, fam y z a, t)
    | NLF.VLam (x, a, t) -> NLF.VLam (x, fam y z a, obj y z t)
    | NLF.VHead (h, p) -> NLF.VHead (head y z h, fatom y z p)

  and obj y z = function
    | NLF.Obj (s, v) -> NLF.Obj (subst y z s, value y z v)

  and subst y z s = Varmap.map
    (function
	| NLF.DAtom (h, l, p) -> NLF.DAtom (head y z h, List.map (value y z) l, fatom y z p)
	| NLF.DHead (h, a) -> NLF.DHead (head y z h, fam y z a)
    ) s

  let rec kind y z = function
    | NLF.KProd (x, a, k) when x=y -> NLF.KProd (x, fam y z a, k)
    | NLF.KProd (x, a, k) -> NLF.KProd (x, fam y z a, kind y z k)
    | NLF.KType -> NLF.KType

end

module HSubst = struct

  let rec fam z c = function
    | NLF.FProd (x, a, b) when x = z -> NLF.FProd (x, fam z c a, b)
    | NLF.FProd (x, a, b) ->
      let y = Name.gen_variable () in
      let b = Refresh.fam x y b in
      NLF.FProd (y, fam z c a, fam z c b)
    | NLF.FAtom p -> NLF.FAtom (fatom z c p)

  and fatom z v (s, c, l) =
    if Varmap.mem z s then (s, c, l) else (* TODO est-ce nécessaire puisque y et Vars(z) st disjoints?? *)
      (subst z v s, c, List.map (value z v) l)

  and value z v = function
    | NLF.VLam (x, a, t) when x = z -> NLF.VLam (x, fam z v a, t)
    | NLF.VLam (x, a, t) ->
      let y = Name.gen_variable () in
      let t = Refresh.obj x y t in
      NLF.VLam (y, fam z v a, obj z v t)
    | NLF.VHead (Cst c, p) -> NLF.VHead (Cst c, fatom z v p)
    | NLF.VHead (Var x, _) when x = z -> v
    | NLF.VHead (Var x, p) -> NLF.VHead (Var x, fatom z v p)

  and subst z v s = Varmap.map (function
    | NLF.DHead (h, a) -> assert false
    | NLF.DAtom (h, l, p) ->
      let p = fatom z v p in
      match h with
	| Cst x -> NLF.DAtom(h, List.map (value z v) l, p)
	| Var x when x <> z -> NLF.DAtom(h, List.map (value z v) l, p)
	| Var x ->
	  args z v (v, l)
  ) s (* TODO verifier que Yann avait raison: il faut rafraichir les noms dans les s *)

  and args z v = function
    | NLF.VHead (h, p), [] ->
      assert false
    | NLF.VLam(x, a, t), w :: l ->
      let y = Name.gen_variable () in
      let t = Refresh.obj x y t in
      let w = value z v w in
      ignore (obj y w t); assert false
    | _ -> assert false 		(* OK, checked by typing *)

  and obj z w = function
    | NLF.Obj (s, v) ->
      if Varmap.mem z s then NLF.Obj(s, v) else (* TODO est-ce nécessaire puisque y et Vars(z) st disjoints?? *)
	NLF.Obj (subst z w s, value z w v)


  let rec kind z v = function
    | NLF.KProd (x, a, k) when x = z -> NLF.KProd (x, fam z v a, k)
    | NLF.KProd (x, a, k) ->
      let y = Name.gen_variable () in
      let k = Refresh.kind x y k in
      NLF.KProd (y, fam z v a, kind z v k)
    | NLF.KType -> NLF.KType

end

module Equals = struct

  let rec fatom sign repo env = function
    | (s, c, m), (s', c', m') ->
      if c <> c'				(* TODO s <> s'? *)
      then failwith ("Not convertible: "^Name.of_fconst c^" <-> "^Name.of_fconst c')
      else args sign repo env (m, m')

  and args sign repo env : _ -> unit = function
    | t :: m, t' :: m' ->
      value sign repo env (t, t');
      args sign repo env (m, m')
    | [], [] -> ()
    | _ -> failwith ("Not convertible 1")

  and value sign repo env = function
    | _ ->
      ()				(* TODO *)

end

let rec head sign repo env : head -> NLF.fam = function
  | Cst c -> Oconstmap.find c (snd sign)
  | Var x ->
    try E.find x env
    with Not_found ->
      try NLF_Utils.lift_def x repo
      with Not_found -> failwith ("not_found "^Name.of_variable x)

and subst sign (NLF.Obj(sigma, v)) env : XLFf.subst -> NLF.subst =
  fun s ->
    List.fold_right
    begin fun (x, (h,l)) sigma ->
      let repo = NLF.Obj(sigma, v) in
      let a = head sign repo env h in
      let l, p = args sign repo env (l, a) in
      S.add x (NLF.DAtom(h,l,p)) sigma
    end s sigma

and args sign repo env : XLFf.args * NLF.fam -> NLF.args * NLF.fatom = function
  | v :: l, NLF.FProd (x, a, b) ->
    let v = value sign repo env (v, a) in
    let l, p = args sign repo env (l, (HSubst.fam x v b)) in
    v :: l, p
  | [], NLF.FAtom p -> [], p
  | _ -> failwith ("args: not applicable")

and fargs sign repo env : XLFf.args * NLF.kind -> NLF.args = function
  | v :: l, NLF.KProd (x, a, k) ->
    let v = value sign repo env (v, a) in
    let l = fargs sign repo env (l, (HSubst.kind x v k)) in
    v :: l
  | [], NLF.KType -> []
  | _ -> failwith ("args: not applicable")

and obj sign repo env : XLFf.obj * NLF.fam -> NLF.obj = function
  | XLFf.Obj(sigma, v), a ->
    let sigma = subst sign repo env sigma in
    let repo = match repo with NLF.Obj(_,v) -> NLF.Obj(sigma,v) in
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
  | XLFf.VLam (x,t), NLF.FProd (y, a, b) ->
    let b = Refresh.fam y x b in
    let t = obj sign repo (E.add x a env) (t, b) in
    NLF.VLam (x, a, t)

  | XLFf.VHead h, NLF.FAtom p ->
    begin match head sign repo env h with
      | NLF.FProd _ -> failwith ("Fct au lieu de valeur")
      | NLF.FAtom p' ->
	Equals.fatom sign repo env (p, p');
	NLF.VHead (h, p)		(* or p' *)
    end
  | XLFf.VLam _, NLF.FAtom _ -> failwith ("Lam attend prod")
  | XLFf.VHead _, NLF.FProd _ -> failwith ("Pas en forme eta-longue")

let rec fam sign repo env = function
  | XLFf.FAtom (s, c, l) ->
    let s = subst sign repo env s in
    let repo = match repo with NLF.Obj(_,v) -> NLF.Obj(s,v) in
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
