open Name

module E = Name.Varmap
module S = Name.Varmap

type eent =
  | EDecl of NLF.fam
  | EDef of NLF.def

let rec equals_fatom sign repo env = function
  | (s, (c, m)), (s', (c', m')) ->
    if c <> c'
    then failwith ("Not convertible: "^Name.of_fconst c^" <-> "^Name.of_fconst c')
    else equals_args sign repo env ((s, m), (s', m'))

and equals_args sign repo env : _ -> unit = function
  | (s, t :: m), (s', t' :: m') ->
    equals_obj sign repo env ((s, t), (s', t'));
    equals_args sign repo env ((s, m), (s', m'))
  | (_, []), (_, []) -> ()
  | _ -> failwith ("Not convertible 1")

and equals_obj sign repo env = function
  | (s, XLF.OAtom (XLF.HConst c, l)), (s', XLF.OAtom (XLF.HConst c', l')) ->
    if c <> c'
    then failwith ("Not convertible: "^Name.of_oconst c^" <-> "^Name.of_oconst c')
    else equals_args sign repo env ((s, l), (s', l'))
  | (s, XLF.OLam (x, t)), (s', XLF.OLam (x', t')) ->
    ()					(* TODO *)
  | (s, XLF.OBox (t, p, u)), (s', XLF.OBox (t', p', u')) ->
    () 					(* TODO (?????) *)
  | _ -> failwith ("Not convertible 2")

let rec ohead sign repo env : XLFf.ohead -> NLF.fam = function
  | XLF.HConst c -> Varmap.empty, Oconstmap.find c (snd sign)
  | XLF.HVar x ->
    try match E.find x env with
      | EDecl a -> a
      | EDef (NLF.DHead(h,a)) -> a
      | EDef (NLF.DAtom(_,_,(b,(c,m)))) -> b, XLF.FAtom(c, m)
    with Not_found ->
      try NLF.lift_def x repo
      with Not_found -> failwith ("not_found "^Name.of_variable x)

and subst sign (NLF.Obj(sigma, _) as repo) env : XLFf.subst -> eent E.t * NLF.def S.t =
  List.fold_left
    begin fun (env, sigma) (x, (h,l)) ->
      let a = ohead sign repo env h in
      let l, (c,m) = args sign repo env (l,a) in
      E.add x (EDef(NLF.DAtom(h,l,(c,m)))) env, S.add x (NLF.DAtom(h,l,(c,m))) sigma
    end (env, sigma)

and args sign repo env : XLFf.args * NLF.fam -> NLF.args * NLF.fatom = function
  | v :: l, (s, XLF.FProd (x, a, b)) ->
    let v = value sign repo env (v, (s, a)) in
    let l, (c, m) = args sign repo env (l, (Varmap.add x v s, b)) in
    (x, v) :: l, (c, m)
  | [], (s, XLF.FAtom (c,l)) ->
    [], (s, (c,l))
  | _ -> failwith ("args: not applicable")

and obj sign repo env : XLFf.obj * NLF.fam -> NLF.obj = function
  | XLFf.Obj(sigma, v), a ->
    let env, sigma = subst sign repo env sigma in
    let v = value sign repo env (v, a) in
    NLF.Obj (sigma, v)
  | XLFf.OBox(t,p,u), a ->
    match NLF.go repo p with
      | NLF.VLam (x, b, repo) ->
	let d = match obj sign repo env (u, b) with
	  | NLF.Obj (sigma, NLF.VHead (h, (s, (a, m)))) -> NLF.DHead(h, (s, XLF.FAtom (a, m))) (* TODO sigma *)
	  | NLF.Obj (sigma, NLF.VLam _) -> failwith "No lambdas in box argument" in
	obj sign (NLF.bind x d repo) env (t, a)
      | _ -> failwith "Position is not a lambda"

and value sign repo env : XLFf.value * NLF.fam -> NLF.value = function
  | XLFf.VLam (x,t), (s, XLF.FProd (y, a, b)) ->
    (* assert (x=y); *)
    let t = obj sign repo (E.add x (EDecl (s, a)) env) (t, (s, b)) in
    NLF.VLam (x, (s, a), t)

  | XLFf.VHead h, (s, XLF.FAtom (c, m)) ->
    begin match ohead sign repo env h with
      | _, XLF.FProd _ -> failwith ("Fct au lieu de valeur")
      | s', XLF.FAtom (c', m') ->
	equals_fatom sign repo env ((s, (c, m)), (s', (c', m')));
	NLF.VHead (h, (s, (c, m)))
    end
  | XLFf.VLam _, (_, XLF.FAtom _) -> failwith ("Lam attend prod")
  | XLFf.VHead _, (_, XLF.FProd _) -> failwith ("Pas en forme eta-longue")

let obj sign repo (t, a) = obj sign repo E.empty (t, (Varmap.empty, a))
