open Name

module P = Position

(* From LF to XLF: sequent-style annotated applications *)

let rec obj l = function
  | LF.OConst c -> XLF.OHead(XLF.HConst c,l)
  | LF.OVar x -> XLF.OHead (XLF.HVar x,l)
  | LF.OApp(t,u) -> obj (obj [] u :: l) t
  | LF.OLam(x,t) ->
      if l = [] then
	XLF.OLam(variable_for x, obj [] t)
      else 
	failwith "redex"
  | LF.OBox(t,p,u) ->
      if l = [] then
	XLF.OBox(obj [] t, p, obj [] u)
      else
	Errors.bad_application (SLF_LF.from_obj t)	(* Product application *)

and fam l = function
  | LF.FConst c -> XLF.FConst(c,l)
  | LF.FProd(x,b,c) as a -> 
      if l = [] then 
	XLF.FProd(variable_for x, fam [] b, fam [] c)
      else 
	Errors.bad_application (SLF_LF.from_fam a)	(* Product application *)
  | LF.FApp(a,t) -> fam (obj [] t :: l) a

let rec kind = function
  | LF.KType -> XLF.KType
  | LF.KProd(x,a,k) -> XLF.KProd(variable_for x, fam [] a, kind k)

let obj t = obj [] t
let fam a = fam [] a
let kind k = kind k

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
  | XLF.OLam (y,t) when x=y -> false
  | XLF.OLam (y,t) -> depends_obj x t
  | XLF.OHead (XLF.HVar y,l) -> if x=y then true else depends_args x l
  | XLF.OHead (XLF.HConst c,l) -> depends_args x l
  | XLF.OBox(t,p,u) -> depends_obj x t || depends_obj x t

let name_for_obj x t = if depends_obj x t then Named x else Anonymous
let name_for_fam x t = if depends_fam x t then Named x else Anonymous
let name_for_kind x t = if depends_kind x t then Named x else Anonymous

let rec from_fapp f = function
  | [] -> f
  | t :: args -> LF.FApp(from_fapp f args, from_obj t)

and from_oapp f = function
  | [] -> f
  | t :: args -> LF.OApp(from_oapp f args, from_obj t)

and from_fam = function
  | XLF.FProd(x,a,b) -> LF.FProd(name_for_fam x b, from_fam a, from_fam b)
  | XLF.FConst(c,l) -> from_fapp (LF.FConst c) l

and from_obj = function
  | XLF.OLam (x,t) -> LF.OLam (name_for_obj x t, from_obj t)
  | XLF.OHead (h,l) -> from_oapp (from_head h) l
  | XLF.OBox(t,p,u) -> LF.OBox(from_obj t, p, from_obj u)

and from_head = function
  | XLF.HVar x -> LF.OVar x
  | XLF.HConst c ->LF.OConst c

let rec from_kind = function
  | XLF.KType -> LF.KType
  | XLF.KProd(x,a,k) -> LF.KProd(name_for_kind x k, from_fam a, from_kind k)

