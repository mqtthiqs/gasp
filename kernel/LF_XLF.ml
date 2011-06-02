open Name

module P = Position

(* From LF to XLF: sequent-style annotated applications *)

let rec obj l = function
  | LF.OConst c -> XLF.OAtom (Cst c, l)
  | LF.OVar x -> XLF.OAtom (Var x, l)
  | LF.OApp(t,u) -> obj (obj [] u :: l) t
  | LF.OLam(x,t) ->
      if l = [] then
	XLF.OLam(x, obj [] t)
      else 
	failwith "redex"
  | LF.OBox(t,p,u) ->
      if l = [] then
	XLF.OBox(obj [] t, p, obj [] u)
      else
	Errors.bad_application (SLF_LF.from_obj t)	(* Box application *)

and fam l = function
  | LF.FConst c -> XLF.FAtom(c,l)
  | LF.FProd(x,b,c) as a -> 
      if l = [] then 
	XLF.FProd(x, fam [] b, fam [] c)
      else 
	Errors.bad_application (SLF_LF.from_fam a)	(* Product application *)
  | LF.FApp(a,t) -> fam (obj [] t :: l) a

let rec kind = function
  | LF.KType -> XLF.KType
  | LF.KProd(x,a,k) -> XLF.KProd(x, fam [] a, kind k)

let obj t = obj [] t
let fam a = fam [] a
let kind k = kind k

(* ... and back *)

let rec from_fapp f = function
  | [] -> f
  | t :: args -> LF.FApp(from_fapp f args, from_obj t)

and from_oapp f = function
  | [] -> f
  | t :: args -> LF.OApp(from_oapp f args, from_obj t)

and from_fam = function
  | XLF.FProd(x,a,b) -> LF.FProd(x, from_fam a, from_fam b)
  | XLF.FAtom(c,l) -> from_fapp (LF.FConst c) l

and from_obj = function
  | XLF.OLam (x,t) -> LF.OLam (x, from_obj t)
  | XLF.OAtom (h,l) -> from_oapp (from_head h) l
  | XLF.OBox(t,p,u) -> LF.OBox(from_obj t, p, from_obj u)

and from_head = function
  | Var x -> LF.OVar x
  | Cst c ->LF.OConst c

let rec from_kind = function
  | XLF.KType -> LF.KType
  | XLF.KProd(x,a,k) -> LF.KProd(x, from_fam a, from_kind k)

