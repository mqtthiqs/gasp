open NLF

(* XLF to XLFa : type annotation on @-spines, argument naming *)

let rec type_of : XLFa.obj -> XLFa.fam = function
  | XLFa.OLam(x,a,t) -> XLFa.FProd(x, a, type_of t)
  | XLFa.OMeta(x,a) -> a
  | XLFa.OHead(_,_,a) -> a

let rec obj genv sign env : XLF.obj -> XLFa.obj = function
  | XLF.OLam (x,a,t) -> 
      let a = fam genv sign env a in
      XLFa.OLam (x, a, obj genv sign ((x,a)::env) t)
  | XLF.OHead (h,l) -> 
      let (h,a) = head genv sign env h in
      let (l,a) = args genv sign env l [] a in
      XLFa.OHead(h,l,a)
  | XLF.OMeta x ->
      let _, _, c, a = NLFSubst.find x genv in
      let a = NLF.Fam(NLFEnv.empty, NLFSubst.empty, c, a) in
      let a = XLFa_XLFe.from_fam (XLFe_XLFn.from_fam (XLFn_NLF.from_fam a)) in
      XLFa.OMeta (x,a)

and head genv sign env = function
  | XLF.HVar x -> XLFa.HVar x, List.assoc x env
  | XLF.HConst c -> 
      let a = match NLFSign.find c sign with
	| NLF.ODecl a -> a
	| NLF.FDecl _ -> assert false in (* bad kinding, checked in LF_XLF *)
      let a = XLFa_XLFe.from_fam (XLFe_XLFn.from_fam (XLFn_NLF.from_fam a)) in
      XLFa.HConst c, a
  | XLF.HApp t -> 
      let t = obj genv sign env t in
      XLFa.HApp t, type_of t

and args genv sign env (l:XLF.args) l' (a:XLFa.fam) : XLFa.args * XLFa.fam = 
  match l,a with
    | [], _ -> l', a
    | t::l, XLFa.FProd(x,a,b) -> 
	args genv sign env l ((x, obj genv sign env t) :: l') b

    (* TODO: La réification pourrait renvoyer des metas à aller chercher à
       la main. Dans ce cas on rajoute un cas: *)
    (* | t::l, Meta x -> va chercher () *)
    | t::l, XLFa.FConst _ -> Errors.over_application (SLF_LF.from_obj (LF_XLF.from_obj t))

and fam genv sign env = function 
  | XLF.FConst(c,l) ->
      let k = match NLFSign.find c sign with
	| NLF.ODecl _ -> assert false    (* bad kinding, checked in LF_XLF *)
	| NLF.FDecl k -> k in
      let k = XLFa_XLFe.from_kind (XLFe_XLFn.from_kind (XLFn_NLF.from_kind k)) in
      let (l,k) = args_fam genv sign env l [] k in
      XLFa.FConst(c,l,k)
  | XLF.FProd(x,a,b) -> 
      let a = fam genv sign env a in
      XLFa.FProd(x,a, fam genv sign ((x,a)::env) b)

and args_fam genv sign env l l' (k:XLFa.kind) =
  match l,k with
    | [], _ -> l', k
    | t::l, XLFa.KProd(x,a,k) ->
	args_fam genv sign env l ((x, obj genv sign env t) :: l') k
    | t::l, XLFa.KType -> Errors.over_application (SLF_LF.from_obj (LF_XLF.from_obj t))

let rec kind genv sign env = function
  | XLF.KType -> XLFa.KType
  | XLF.KProd(x,a,k) -> 
      let a = fam genv sign env a in
      XLFa.KProd(x, a, kind genv sign ((x,a)::env) k)

let entry kont nlfs = function 
    | XLF.ODecl a -> kont nlfs (XLFa.ODecl (fam NLFSubst.empty nlfs [] a))
    | XLF.FDecl k -> kont nlfs (XLFa.FDecl (kind NLFSubst.empty nlfs [] k))

let sign kont nlfs xlfs =
    List.fold_left
      (fun nlfs (c,t) -> 
	 NLFSign.add c (entry kont nlfs t) nlfs
      ) nlfs xlfs

let obj genv sign = obj genv sign []
let fam genv sign = fam genv sign []
let kind genv sign = kind genv sign []


(* ... and back *)

let rec from_obj = function
  | XLFa.OLam(x,a,t) -> XLF.OLam(x, from_fam a, from_obj t)
  | XLFa.OHead(h,l,_) -> XLF.OHead(from_head h, from_args l)
  | XLFa.OMeta (x,a) -> XLF.OMeta (x,a)

and from_head = function
  | XLFa.HVar x -> XLF.HVar x
  | XLFa.HConst c -> XLF.HConst c
  | XLFa.HApp t -> XLF.HApp (from_obj t)

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
