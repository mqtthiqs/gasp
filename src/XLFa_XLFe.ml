(* From XLF to XLFe (eta-expansion) *)

let rec obj = function
  | XLFa.OLam(x,a,t) -> XLFe.OLam(x, fam a, obj t)
  | XLFa.OVar(x,l,XLFa.FProd(y,a,b)) -> 
      XLFe.OLam(y, fam a, obj (XLFa.OVar(x, l@[y, XLFa.OVar(y, [], a)], b)))
  | XLFa.OVar(x,l,XLFa.FConst(c,l',k)) -> 
      XLFe.OHead(XLFe.OVar(x, args l, XLFe.FConst(c, args l', khead k)))

  | XLFa.OMeta(x,l,XLFa.FProd(y,a,b)) -> 
      XLFe.OLam(y, fam a, obj (XLFa.OMeta(x, l@[y, XLFa.OMeta(y, [], a)], b))) (* TODO erreur *)
  | XLFa.OMeta(x,l,XLFa.FConst(c,l',k)) -> 
      XLFe.OHead(XLFe.OMeta(x, args l, XLFe.FConst(c, args l', khead k)))


  | XLFa.OConst(c,l,XLFa.FProd(y,a,b)) -> 
      XLFe.OLam(y, fam a, obj (XLFa.OConst(c, l@[y, XLFa.OVar(y, [], a)], b)))
  | XLFa.OConst(c,l,XLFa.FConst(d,l',k)) -> 
      XLFe.OHead(XLFe.OConst(c, args l, XLFe.FConst(d, args l', khead k)))
  | XLFa.OApp(t,l,XLFa.FProd(y,a,b)) -> 
      XLFe.OLam(y, fam a, obj (XLFa.OApp(t, l@[y, XLFa.OVar(y, [], a)], b)))
  | XLFa.OApp(t,l,XLFa.FConst(c,l',k)) -> 
      XLFe.OHead(XLFe.OApp(obj t, args l, XLFe.FConst(c, args l', khead k)))

and args l = List.map (fun (x,t) -> x, obj t) l

and fam = function
  | XLFa.FProd(x,a,b) -> XLFe.FProd(x, fam a, fam b)
  | XLFa.FConst(c,l,XLFa.KType) -> XLFe.FHead(XLFe.FConst(c, args l, XLFe.KType))
  | XLFa.FConst(c,l,_) -> assert false 	(* on peut pas faire d'eta sur fam *)

and khead = function
  | XLFa.KType -> XLFe.KType
  | _ -> assert false			(* ??? *)

and kind = function
  | XLFa.KType -> XLFe.KHead(XLFe.KType)
  | XLFa.KProd(x,a,k) -> XLFe.KProd(x, fam a, kind k)

let rec sign s =
  List.map
    (function
       | c, XLFa.FDecl k -> c, XLFe.FDecl (kind k)
       | c, XLFa.ODecl a -> c, XLFe.ODecl (fam a)
    ) s

(* ... and back *)

let rec from_obj : XLFe.obj -> XLFa.obj = function
  | XLFe.OLam(x,a,t) -> XLFa.OLam(x, from_fam a, from_obj t)
  | XLFe.OHead h -> from_ohead h

and from_args l = List.map (fun (x,t) -> x, from_obj t) l

and from_ohead : XLFe.ohead -> XLFa.obj = function
  | XLFe.OVar(x,l,a) -> XLFa.OVar(x, from_args l, from_fhead a)
  | XLFe.OConst(c,l,a) -> XLFa.OConst(c, from_args l, from_fhead a)
  | XLFe.OApp(t,l,a) -> XLFa.OApp(from_obj t, from_args l, from_fhead a)
  | XLFe.OMeta _ -> assert false

and from_fhead : XLFe.fhead -> XLFa.fam = function
  | XLFe.FConst (c,l,XLFe.KType) -> XLFa.FConst(c,from_args l, XLFa.KType)

and from_fam : XLFe.fam -> XLFa.fam = function
  | XLFe.FProd(x,a,b) -> XLFa.FProd(x, from_fam a, from_fam b)
  | XLFe.FHead h -> from_fhead h

let rec from_kind = function
  | XLFe.KProd(x,a,k) -> XLFa.KProd(x,from_fam a,from_kind k)
  | XLFe.KHead(XLFe.KType) -> XLFa.KType
 
let rec from_sign s = 
  List.map
    (function
       | c, XLFe.ODecl a -> c, XLFa.ODecl(from_fam a)
       | c, XLFe.FDecl k -> c, XLFa.FDecl(from_kind k)
    ) s
