open NLF

(* XLF to XLFa : type annotation on @-spines, argument naming *)

let rec type_of : XLFa.obj -> XLFa.fam = function
  | XLFa.OLam(x,a,t) -> XLFa.FProd(x, a, type_of t)
  | XLFa.OVar(_,_,a) | XLFa.OConst(_,_,a) | XLFa.OApp(_,_,a) | XLFa.OMeta(_,_,a) -> a


let rec obj genv sign env : XLF.obj -> XLFa.obj = function
  | XLF.OLam (x,a,t) -> 
      let a = fam genv sign env a in
      XLFa.OLam (x, a, obj genv sign ((x,a)::env) t)
  | XLF.OVar (x,l) -> 
      let (l,a) = args genv sign env l [] (List.assoc x env) in
      XLFa.OVar(x,l,a)
  | XLF.OMeta (x,l) -> 			(* Detranslation of the type found in the genv *)
      let a = match NLFEnv.find genv x with
	| NLFEnv.ODecl a -> a
	| NLFEnv.ODef t -> lift t in
      let a = XLFa_XLFe.from_fam (XLFe_NLF.from_fam a) in
      let (l,a) = args genv sign env l [] a in
      XLFa.OMeta(x,l,a)
  | XLF.OConst (c,l) ->
      let a = match NLFSign.find c sign with
	| NLFSign.ODecl a -> a
	| NLFSign.FDecl _ -> assert false in (* OK *)
      let a = XLFa_XLFe.from_fam (XLFe_NLF.from_fam a) in
      let (l,a) = args genv sign env l [] a in
      XLFa.OConst(c,l,a)
  | XLF.OApp (t,l) ->
      let t = obj genv sign env t in
      let (l,a) = args genv sign env l [] (type_of t) in
      XLFa.OApp(t, l, a)

and args genv sign env (l:XLF.args) l' (a:XLFa.fam) : XLFa.args * XLFa.fam = 
  match l,a with
    | [], _ -> l', a
    | t::l, XLFa.FProd(x,a,b) -> 
	args genv sign env l ((x, obj genv sign env t) :: l') b

    (* La réification pourrait renvoyer des metas à aller chercher à
       la main. Dans ce cas on rajoute un cas: *)
    (* | t::l, Meta x -> va chercher () *)
    | t::l, XLFa.FConst _ -> assert false  (* over app, checked in LF_XLF *)

and fam genv sign env = function 
  | XLF.FConst(c,l) ->
      let k = match NLFSign.find c sign with
	| NLFSign.ODecl _ -> assert false    (* OK *)
	| NLFSign.FDecl k -> k in
      let k = XLFa_XLFe.from_kind (XLFe_NLF.from_kind k) in
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
    | t::l, XLFa.KType -> assert false  (* over app, checked in LF_XLF *)

let rec kind genv sign env = function
  | XLF.KType -> XLFa.KType
  | XLF.KProd(x,a,k) -> 
      let a = fam genv sign env a in
      XLFa.KProd(x, a, kind genv sign ((x,a)::env) k)

let entry kont nlfs = function 
    | XLF.ODecl a -> kont nlfs (XLFa.ODecl (fam NLFEnv.empty nlfs [] a))
    | XLF.FDecl k -> kont nlfs (XLFa.FDecl (kind NLFEnv.empty nlfs [] k))

let sign kont nlfs xlfs =
    List.fold_left
      (fun nlfs (c,t) -> 
	 NLFSign.add c (entry kont nlfs t) nlfs
      ) nlfs xlfs


(* sign (s' : XLFa.sign) (s :XLF.sign) : XLFa.sign =  *)
(*   Util.list_map_prefix  *)
(*     (fun s -> function *)
(*        | c, XLF.ODecl a -> c, XLFa.ODecl (fam NLFEnv.empty s [] a) *)
(*        | c, XLF.FDecl k -> c, XLFa.FDecl (kind NLFEnv.empty s [] k) *)
(*     ) s' s *)


let obj genv sign = obj genv sign []
let fam genv sign = fam genv sign []
let kind genv sign = kind genv sign []


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
