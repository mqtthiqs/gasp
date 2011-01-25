(* XLF to XLFa : type annotation on @-spines, argument naming *)

let rec type_of : XLFa.obj -> XLFa.fam = function
  | XLFa.OLam(x,a,t) -> XLFa.FProd(x, a, type_of t)
  | XLFa.OVar(_,_,a) | XLFa.OConst(_,_,a) | XLFa.OApp(_,_,a) | XLFa.OMeta(_,_,a) -> a
 

let rec obj repo sign env : XLF.obj -> XLFa.obj = function
  | XLF.OLam (x,a,t) -> 
      let a = fam repo sign env a in
      XLFa.OLam (x, a, obj repo sign ((x,a)::env) t)
  | XLF.OVar (x,l) -> 
      let (l,a) = args repo sign env l [] (List.assoc x env) in
      XLFa.OVar(x,l,a)
  | XLF.OMeta (x,l) -> 
      let a = match NLF.NLFEnv.find repo x with
	| NLF.NLFEnv.ODecl a -> a
	| NLF.NLFEnv.ODef t -> NLF.lift t in
      let a = XLFa_XLFe.from_fam (XLFe_NLF.from_fam a) in
      let (l,a) = args repo sign env l [] a in
      XLFa.OMeta(x,l,a)
  | XLF.OConst (c,l) ->
      let (l,a) = args repo sign env l [] (match List.assoc c sign with
					  XLFa.ODecl a -> a
					| XLFa.FDecl _ -> assert false) in
      XLFa.OConst(c,l,a)
  | XLF.OApp (t,l) ->
      let t = obj repo sign env t in
      let (l,a) = args repo sign env l [] (type_of t) in
      XLFa.OApp(t, l, a)

and args repo sign env (l:XLF.args) l' (a:XLFa.fam) : XLFa.args * XLFa.fam = 
  match l,a with
    | [], _ -> l', a
    | t::l, XLFa.FProd(x,a,b) -> 
	args repo sign env l ((x, obj repo sign env t) :: l') b
    | t::l, XLFa.FConst _ -> assert false  (* over app, checked in LF_XLF *)

and fam repo sign env = function 
  | XLF.FConst(c,l) ->
      let (l,a) = args_fam repo sign env l [] (match List.assoc c sign with
					      XLFa.ODecl _ -> assert false
					    | XLFa.FDecl k -> k) in
      XLFa.FConst(c,l,a)
  | XLF.FProd(x,a,b) -> 
      let a = fam repo sign env a in
      XLFa.FProd(x,a, fam repo sign ((x,a)::env) b)

and args_fam repo sign env l l' (k:XLFa.kind) =
  match l,k with
    | [], _ -> l', k
    | t::l, XLFa.KProd(x,a,k) ->
	args_fam repo sign env l ((x, obj repo sign env t) :: l') k
    | t::l, XLFa.KType -> assert false  (* over app, checked in LF_XLF *)

let rec kind repo sign env = function
  | XLF.KType -> XLFa.KType
  | XLF.KProd(x,a,k) -> 
      let a = fam repo sign env a in
      XLFa.KProd(x, a, kind repo sign ((x,a)::env) k)

let sign repo (s' : XLFa.sign) (s :XLF.sign) : XLFa.sign = 
  Util.list_map_prefix 
    (fun s -> function
       | c, XLF.ODecl a -> c, XLFa.ODecl (fam repo s [] a)
       | c, XLF.FDecl k -> c, XLFa.FDecl (kind repo s [] k)
    ) s' s


(* ... and back *)

let rec from_obj = function
  | XLFa.OLam(x,a,t) -> XLF.OLam(x, from_fam a, from_obj t)
  | XLFa.OVar(x,l,a) -> XLF.OVar(x, from_args l)
  | XLFa.OConst(c,l,a) -> XLF.OConst(c, from_args l)
  | XLFa.OApp(t,l,a) -> XLF.OApp(from_obj t, from_args l)
  | XLFa.OMeta(x,l,a) -> XLF.OMeta (x, from_args l)

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
