open Name

module E = Name.Varmap
module S = Name.Varmap

type eent =
  | EDecl of NLF.fam
  | EDef of NLF.def

let rec hsubst_fam x v a b : XLF.fam = b (* TODO *)
let rec hsubst_kind x v a k : XLF.kind = k (* TODO *)

let rec equals_args sign repo env = function
  | v :: m, v' :: m' ->
    equals_val sign repo env (v, v');
    equals_args sign repo env (m, m')
  | [], [] -> ()
  | _ -> failwith ("Not convertible 1")

and equals_val sign repo env = function
  (* | NLF.VHead (h,_,_), NLF.VHead (h',_,_) -> *)
  (*   (\* assert (h=h') 			(\\* TODO *\\) *\) *)
  (*   () *)
  (* | NLF.VLam (x, _, t), NLF.VLam (x', _, t') -> *)
  (*   (\* assert (x=x');			(\\* TODO *\\) *\) *)
  (*   (\* assert (t=t') *\) *)
  (*   () *)
  | _ -> failwith ("Not convertible 2")

let rec ohead sign repo env : XLFf.ohead -> NLF.fam = function
  | XLF.HConst c -> Oconstmap.find c (snd sign)
  | XLF.HVar x ->
    try match E.find x env with
      | EDecl a -> a
      | EDef (NLF.DHead(h,a)) -> a
      | EDef (NLF.DApp(_,_,(c,m))) -> XLF.FConst(c, m)
    with Not_found ->
      try NLF.lift_def x repo
      with Not_found -> failwith ("not_found "^Name.of_variable x)

and subst sign (NLF.Obj(sigma, _) as repo) env : XLFf.subst -> eent E.t * NLF.def S.t =
  List.fold_left
    begin fun (env, sigma) (x, (h,l)) ->
      let a = ohead sign repo env h in
      let l, (c,m) = args sign repo env (l,a) in
      E.add x (EDef(NLF.DApp(h,l,(c,m)))) env, S.add x (NLF.DApp(h,l,(c,m))) sigma
    end (env, sigma)

and args sign repo env : XLFf.args * NLF.fam -> NLF.args * (fconst * XLF.args) = function
  | v :: l, XLF.FProd (x, a, b) ->
    let v = value sign repo env (v, a) in
    let l, (c, m) = args sign repo env (l, hsubst_fam x v a b) in
    (x, v) :: l, (c, m)
  | [], XLF.FConst (h,l) ->
    [], (h,l)
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
	  | NLF.Obj (sigma, NLF.VHead (h, (a, m))) -> NLF.DHead(h, XLF.FConst (a, m))
	  | NLF.Obj (sigma, NLF.VLam _) -> failwith "No lambdas in box argument" in
	obj sign (NLF.bind x d repo) env (t, a)

      | _ -> failwith "Position is not a lambda"

and value sign repo env : XLFf.value * NLF.fam -> NLF.value = function
  | XLFf.VLam (x,t), XLF.FProd (y, a, b) ->
    (* assert (x=y); *)
    let t = obj sign repo (E.add x (EDecl a) env) (t, b) in
    NLF.VLam (x, a, t)

  | XLFf.VHead h, XLF.FConst (c, m) ->
    begin match ohead sign repo env h with
      | XLF.FProd _ -> failwith ("Fct au lieu de valeur")
      | XLF.FConst (c', m') ->
	if c <> c' then failwith ("Not convertible: "^Name.of_fconst c^" <-> "^Name.of_fconst c');
	equals_args sign repo env (m, m');
	NLF.VHead (h, (c, m))
    end
  | XLFf.VLam _, XLF.FConst _ -> failwith ("Lam attend prod")
  | XLFf.VHead _, XLF.FProd _ -> failwith ("Pas en forme eta-longue")

let obj sign repo = obj sign repo E.empty
