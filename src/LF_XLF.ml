open Name

module P = Position

(* From LF to XLF: sequent-style annotated applications *)

let rec obj l t : XLF.obj = 
  match P.value t with
    | LF.OConst c -> XLF.OConst(c,l)
    | LF.OVar x -> XLF.OVar(x,l)
    | LF.OMeta x -> XLF.OMeta(x,l)
    | LF.OApp(t,u) -> obj (obj [] u :: l) t
    | LF.OLam(x,a,t) -> 
	if l = [] then
	  XLF.OLam(variable_for x, fam [] a, obj [] t)
	else 
	  XLF.OApp(XLF.OLam(variable_for x, fam [] a,obj [] t), l)

and fam l a : XLF.fam = 
  match P.value a with
    | LF.FConst c -> XLF.FConst(c,l)
    | LF.FProd(x,b,c) -> 
	if l = [] then 
	  XLF.FProd(variable_for x, fam [] b, fam [] c)
	else 
	  Errors.bad_application (SLF_LF.from_fam a)	(* Product application *)
    | LF.FApp(a,t) -> fam (obj [] t :: l) a

let rec kind k : XLF.kind = 
  match P.value k with
    | LF.KType -> XLF.KType
    | LF.KProd(x,a,k) -> XLF.KProd(variable_for x, fam [] a, kind k)

let entry kont nlfs = function
  | LF.FDecl k -> kont nlfs (XLF.FDecl (kind k))
  | LF.ODecl k -> kont nlfs (XLF.ODecl (fam [] k))

(* ... and back *)

let rec depends_kind x = function
  | XLF.KType -> false
  | XLF.KProd (y,a,k) when x=y -> false
  | XLF.KProd (y,a,k) -> depends_fam x a || depends_kind x k
and depends_fam x = function
  | XLF.FProd (y,a,b) when x=y -> false
  | XLF.FProd (y,a,b) -> depends_fam x a || depends_fam x b
  | XLF.FConst (c,l) -> depends_args x l
and depends_args x l = List.exists (depends_obj x) l
and depends_obj x = function
  | XLF.OLam (y,a,t) when x=y -> depends_fam x a
  | XLF.OLam (y,a,t) -> depends_fam x a || depends_obj x t
  | XLF.OVar (y,l) when x=y -> true
  | XLF.OVar (y,l) | XLF.OMeta(y,l) -> depends_args x l
  | XLF.OConst (c,l) -> depends_args x l
  | XLF.OApp (t,l) -> depends_obj x t || depends_args x l

let name_for_obj x t = if depends_obj x t then Named x else Anonymous
let name_for_fam x t = if depends_fam x t then Named x else Anonymous
let name_for_kind x t = if depends_kind x t then Named x else Anonymous

let name_for_obj x t = Named x
let name_for_fam x t = Named x
let name_for_kind x t = Named x

let rec from_fapp' f = function
  | [] -> f
  | t :: args -> LF.FApp(from_fapp f args, from_obj t)

and from_fapp f args = P.with_pos P.dummy (from_fapp' f args)

and from_oapp' f = function
  | [] -> f
  | t :: args -> LF.OApp(from_oapp f args, from_obj t)

and from_oapp f args = P.with_pos P.dummy (from_oapp' f args)

and from_fam' = function
  | XLF.FProd(x,a,b) -> LF.FProd(name_for_fam x b, from_fam a, from_fam b)
  | XLF.FConst(c,l) -> from_fapp' (LF.FConst c) l

and from_fam a : LF.fam = P.with_pos P.dummy (from_fam' a)

and from_obj' = function
  | XLF.OLam (x,a,t) -> LF.OLam (name_for_obj x t, from_fam a, from_obj t)
  | XLF.OVar (x,l) -> from_oapp' (LF.OVar x) l
  | XLF.OMeta (x,l) -> from_oapp' (LF.OMeta x) l
  | XLF.OConst (c,l) -> from_oapp' (LF.OConst c) l
  | XLF.OApp (t,l) -> from_oapp' (from_obj' t) l

and from_obj t = P.with_pos P.dummy (from_obj' t)

let rec from_kind' = function
  | XLF.KType -> LF.KType
  | XLF.KProd(x,a,k) -> LF.KProd(name_for_kind x k, from_fam a, from_kind k)

and from_kind t = P.with_pos P.dummy (from_kind' t)

let rec from_sign = 
  List.map 
    (function
       | x, XLF.FDecl k -> x, LF.FDecl (from_kind k)
       | x, XLF.ODecl a -> x, LF.ODecl (from_fam a)
    )

