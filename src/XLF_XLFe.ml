open Name

(* From XLF to XLFe (eta-expansion) *)

let rec obj = function
  | XLF.OLam(x,a,t) -> XLFe.OLam(variable_for x, fam a, obj t)
  | XLF.OVar(x,l,XLF.FProd(y,a,b)) -> 
      let y = variable_for y in
      XLFe.OLam(y, fam a, obj (XLF.OVar(x, l@[XLF.OVar(y, [], a)], b)))
  | XLF.OVar(x,l,XLF.FConst(c,l',k)) -> 
      XLFe.OHead(XLFe.OVar(x, List.map obj l, XLFe.FConst(c, List.map obj l', khead k)))
  | XLF.OConst(c,l,XLF.FProd(y,a,b)) -> 
      let y = variable_for y in
      XLFe.OLam(y, fam a, obj (XLF.OConst(c, l@[XLF.OVar(y, [], a)], b)))
  | XLF.OConst(c,l,XLF.FConst(d,l',k)) -> 
      XLFe.OHead(XLFe.OConst(c, List.map obj l, XLFe.FConst(d, List.map obj l', khead k)))
  | XLF.OApp(t,l,XLF.FProd(y,a,b)) -> 
      let y = variable_for y in
      XLFe.OLam(y, fam a, obj (XLF.OApp(t, l@[XLF.OVar(y, [], a)], b)))
  | XLF.OApp(t,l,XLF.FConst(c,l',k)) -> 
      XLFe.OHead(XLFe.OApp(obj t, List.map obj l, XLFe.FConst(c, List.map obj l', khead k)))

and fam = function
  | XLF.FProd(x,a,b) -> XLFe.FProd(variable_for x, fam a, fam b)
  | XLF.FConst(c,l,XLF.KType) -> XLFe.FHead(XLFe.FConst(c, List.map obj l, XLFe.KType))
  | XLF.FConst(c,l,_) -> assert false 	(* on peut pas faire d'eta sur fam *)

and khead = function
  | XLF.KType -> XLFe.KType
  | _ -> assert false			(* ??? *)

and kind = function
  | XLF.KType -> XLFe.KHead(XLFe.KType)
  | XLF.KProd(x,a,k) -> XLFe.KProd(variable_for x, fam a, kind k)

let rec sign = function
  | [] -> []
  | (c, XLF.FDecl k) :: s -> (c, XLFe.FDecl (kind k)) :: sign s
  | (c, XLF.ODecl a) :: s -> (c, XLFe.ODecl (fam a)) :: sign s

(* ... and back *)

let rec from_obj : XLFe.obj -> XLF.obj = function
  | XLFe.OLam(x,a,t) -> XLF.OLam(Named x, from_fam a, from_obj t)
  | XLFe.OHead h -> from_ohead h

and from_ohead : XLFe.ohead -> XLF.obj = function
  | XLFe.OVar(x,l,a) -> XLF.OVar(x, List.map from_obj l, from_fhead a)
  | XLFe.OConst(c,l,a) -> XLF.OConst(c, List.map from_obj l, from_fhead a)
  | XLFe.OApp(t,l,a) -> XLF.OApp(from_obj t, List.map from_obj l, from_fhead a)

and from_fhead : XLFe.fhead -> XLF.fam = function
  | XLFe.FConst (c,l,XLFe.KType) -> XLF.FConst(c,List.map from_obj l, XLF.KType)

and from_fam : XLFe.fam -> XLF.fam = function
  | XLFe.FProd(x,a,b) -> XLF.FProd(Named x, from_fam a, from_fam b)
  | XLFe.FHead h -> from_fhead h

let rec from_kind = function
  | XLFe.KProd(x,a,k) -> XLF.KProd(Named x,from_fam a,from_kind k)
  | XLFe.KHead(XLFe.KType) -> XLF.KType
 
let rec from_sign s = 
  List.map
    (function
       | c, XLFe.ODecl a -> c, XLF.ODecl(from_fam a)
       | c, XLFe.FDecl k -> c, XLF.FDecl(from_kind k)
    ) s
