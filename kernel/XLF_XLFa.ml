module E = Name.Varmap

(* XLF to XLFa : type annotation on @-spines, argument naming *)

let reify_fam a = XLFa_XLFe.from_fam (XLFe_NLF.from_fam a)
let reify_kind a = XLFa_XLFe.from_kind (XLFe_NLF.from_kind a)

let rec type_of : XLFa.obj -> XLFa.fam = function
  | XLFa.OLam(x,a,t) -> XLFa.FProd(x, a, type_of t)
  | XLFa.OMeta(x,a) -> a
  | XLFa.OHead(_,_,a) -> a
  | XLFa.OBox(t,_,_) -> type_of t

let rec obj sign term env : XLF.obj -> XLFa.obj = function
  | XLF.OLam (x,a,t) -> 
      let a = fam sign term env a in
      XLFa.OLam (x, a, obj sign term (E.add x a env) t)
  | XLF.OHead (h,l) ->
    begin
      match h with
	| XLF.HConst c ->
	  let a = try match NLF.NLFSign.find c sign with
	    | NLF.NLF.ODecl a -> reify_fam a
	    | NLF.NLF.FDecl _ -> assert false (* bad kinding, checked in LF_XLF *)
	    with Not_found -> assert false in (* constant not found, checked in LF_XLF *)
	  let l, a = args sign term env l [] a in
	  XLFa.OHead(XLF.HConst c, l, a)
	| XLF.HVar x ->
	  try
	    let a = E.find x env in
	    let l, a = args sign term env l [] a in
	    XLFa.OHead(XLF.HVar x, l, a)
	  with Not_found ->
	    try
	      let a = reify_fam (NLF.lift_def x term) in
	      if l = [] then XLFa.OMeta(x, a)
	      else Errors.bad_application (SLF_LF.from_obj (LF_XLF.from_obj (XLF.OHead(h,l))))
	    with Not_found -> Errors.not_bound (Position.dummy) x
    end
  | XLF.OBox(t,p,u) ->
    let s = obj sign term env u in
    let u = NLF.to_def (XLFe_NLF.obj term (XLFa_XLFe.obj s)) in
    let term = NLF.go term p u in
    XLFa.OBox(obj sign term env t, p, s) (* TODO subst!*)

and args sign term env (l:XLF.args) l' (a:XLFa.fam) : XLFa.args * XLFa.fam = 
  match l,a with
    | [], _ -> l', a
    | t::l, XLFa.FProd(x,a,b) ->
      let t' = obj sign term env t in
      (* TODO: comparer les types a et type_of(t') ici *)
      args sign term env l ((x, t') :: l') b
    (* TODO: La réification pourrait renvoyer des metas à aller chercher à
       la main. Dans ce cas on rajoute un cas: *)
    (* | t::l, Meta x -> va chercher () *)
    | t::l, XLFa.FConst _ -> Errors.over_application (SLF_LF.from_obj (LF_XLF.from_obj t))

and fam sign term env = function 
  | XLF.FConst(c,l) ->
      let k = match NLF.NLFSign.find c sign with
	| NLF.NLF.ODecl _ -> assert false    (* bad kinding, checked in LF_XLF *)
	| NLF.NLF.FDecl k -> k in
      let k = reify_kind k in
      let (l,k) = args_fam sign term env l [] k in
      XLFa.FConst(c,l,k)
  | XLF.FProd(x,a,b) -> 
      let a = fam sign term env a in
      XLFa.FProd(x,a, fam sign term (E.add x a env) b)

and args_fam sign term env l l' (k:XLFa.kind) =
  match l,k with
    | [], _ -> l', k
    | t::l, XLFa.KProd(x,a,k) ->
	args_fam sign term env l ((x, obj sign term env t) :: l') k
    | t::l, XLFa.KType -> Errors.over_application (SLF_LF.from_obj (LF_XLF.from_obj t))

let rec kind sign term env = function
  | XLF.KType -> XLFa.KType
  | XLF.KProd(x,a,k) -> 
      let a = fam sign term env a in
      XLFa.KProd(x, a, kind sign term (E.add x a env) k)

let entry kont nlfs = function 
    | XLF.ODecl a -> kont nlfs (XLFa.ODecl (fam nlfs NLF.bidon E.empty a))
    | XLF.FDecl k -> kont nlfs (XLFa.FDecl (kind nlfs NLF.bidon E.empty k))

let sign kont nlfs xlfs =
    List.fold_left
      (fun nlfs (c,t) -> 
	 NLF.NLFSign.add c (entry kont nlfs t) nlfs
      ) nlfs xlfs

let obj genv sign = obj genv sign E.empty
let fam genv sign = fam genv sign E.empty
let kind genv sign = kind genv sign E.empty


(* ... and back *)

let rec from_obj = function
  | XLFa.OLam(x,a,t) -> XLF.OLam(x, from_fam a, from_obj t)
  | XLFa.OHead(h,l,_) -> XLF.OHead(h, from_args l)
  | XLFa.OMeta (x,_) -> XLF.OHead(XLF.HVar x, [])
  | XLFa.OBox(t,p,u) -> XLF.OBox(from_obj t, p, from_obj u)

and from_args l = List.map (fun (_,t) -> from_obj t) l
and from_fam = function
  | XLFa.FConst(c,l,_) -> XLF.FConst(c,from_args l)
  | XLFa.FProd(x,a,b) -> XLF.FProd(x,from_fam a, from_fam b)

let rec from_kind = function
  | XLFa.KType -> XLF.KType
  | XLFa.KProd(x,a,k) -> XLF.KProd(x, from_fam a, from_kind k)

let from_sign sign = List.map (function
				 | c, XLFa.FDecl k -> c, XLF.FDecl (from_kind k)
				 | c, XLFa.ODecl a -> c, XLF.ODecl (from_fam a)) sign
