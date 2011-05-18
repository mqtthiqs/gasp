include types of mli with

module Pp = struct
  open Format
  open Print
  open Name.Pp

  type entity = 
    | K of kind
    | F of fam
    | O of obj
    | H of ohead
    | B of subst
    | A of args
    | V of value

  let ent_prec = function
      _ -> 10

 let pp pp : entity printing_fun =
    let pr_fhead fmt (c, l) : unit =
      if l = [] then fconst fmt c else
    	fprintf fmt "@[%a@ %a@]" fconst c (pp (<=)) (A l) in
    fun fmt (e:entity) -> match e with
    | K(KType) -> fprintf fmt "@[type@]"
    | K(KProd(x,a,k)) ->
      fprintf fmt "@[Π%a@ :@ %a. %a@]" variable x (pp (<=)) (F a) (pp (<=)) (K k)
    | F(FHead(s,c,l)) when s = [] -> fprintf fmt "@[%a@]" pr_fhead (c, l)
    | F(FHead(s,c,l)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) pr_fhead (c, l)
    | F(FProd(x,a,b)) ->
      fprintf fmt "@[Π%a@ :@ %a. %a@]" variable x (pp (<=)) (F a) (pp (<=)) (F b)
    | O(Obj(s, v)) when s = [] -> pp (<=) fmt (V v)
    | O(Obj(s, v)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) (pp (<=)) (V v)
    | O(OBox(t,p,u)) -> assert false
    | H(XLF.HVar x) -> variable fmt x
    | H(XLF.HConst c) -> oconst fmt c
    | A a -> pr_list pr_spc (fun fmt a -> fprintf fmt "@[%a@]" (pp (<=)) (V a)) fmt a
    | B b -> List.fold_left
      begin fun () (x, (h,l)) ->
	fprintf fmt "@[[%a@ =@ %a@ %a]@]@," variable x (pp (<=)) (H h) (pp (<=)) (A l)
      end () b
    | V(VHead h) -> fprintf fmt "@[%a@]" (pp (<=)) (H h)
    | V(VLam (x,t)) -> fprintf fmt "@[λ%a.@ %a@]" variable x (pp (<=)) (O t)

  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
  let fam fmt s = pr_paren pp ent_prec 100 (<=) fmt (F s)
  let kind fmt s = pr_paren pp ent_prec 100 (<=) fmt (K s)
end


and module Check = struct
  open NLF

  type eent =
    | EDecl of NLF.fam
    | EDef of NLF.def

  module E = Name.Varmap
  module S = NLFSubst

  let compare x y  = assert false
  let rec hereditary_subst x v a b = assert false

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
      arg sign env repo sigma (v, a);
      args sign env repo sigma (l, hereditary_subst x v a b)
    | [], NLF.FHead (sigma',h,l) ->
      assert false, (h,l)
    | _ -> failwith ("args: not applicable")

  and fargs sign env repo sigma : args * NLF.kind -> NLF.args = function
    | v :: l, NLF.KProd (x, a, b) ->
      let v = arg sign env repo sigma (v, a) in
      fargs sign env repo sigma (l, hereditary_subst x v a b)
    | [], NLF.KType -> assert false
    | _ -> failwith ("fargs: not applicable")

  and obj sign env repo : obj -> NLF.fam = function
    | Obj(sigma, v) ->
      let env, sigma = subst sign env repo sigma in
      value sign env repo v
    | OBox(t,p,u) -> assert false

  and value sign env repo : value -> NLF.fam = function
    | VHead h -> ohead sign env repo h
    | VLam (x,t) -> obj sign env repo t

  and arg sign env repo sigma : value * NLF.fam -> unit = function
    | VLam (x,t), NLF.FProd (y, a, b) ->
      assert (x=y);
      let b' = obj sign (E.add x (EDecl a) env) repo t in
      compare b b'
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

end
