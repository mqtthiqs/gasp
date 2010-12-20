open Name

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
