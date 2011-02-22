open NLF

(* From XLF to XLFe (eta-expansion) *)

let rec obj = function
  | XLFa.OLam(x,a,t) -> XLFe.OLam(x, fam a, obj t)
  | XLFa.OHead(h,l,XLFa.FProd(y,a,b)) ->
      XLFe.OLam(y, fam a, obj (XLFa.OHead(h, (y, XLFa.OHead(XLFa.HVar y, [], a)) :: l, b)))
  | XLFa.OHead(h,l,XLFa.FConst(c,l',k)) ->
      XLFe.OHead(ohead h, args l, XLFe.FConst(c, args l'))

and ohead = function
  | XLFa.HVar x -> XLFe.HVar x
  | XLFa.HConst c -> XLFe.HConst c
  | XLFa.HApp t -> XLFe.HApp (obj t)
  | XLFa.HMeta x -> XLFe.HMeta x

and args l = List.map (fun (x,t) -> x, obj t) l

and fam = function
  | XLFa.FProd(x,a,b) -> XLFe.FProd(x, fam a, fam b)
  | XLFa.FConst(c,l,XLFa.KType) -> XLFe.FHead(XLFe.FConst(c, args l))
  | XLFa.FConst(c,l,_) -> assert false 	(* on peut pas faire d'eta sur fam *)

and kind = function
  | XLFa.KType -> XLFe.KType
  | XLFa.KProd(x,a,k) -> XLFe.KProd(x, fam a, kind k)

let entry kont nlfs = function 
    | XLFa.ODecl a -> kont nlfs (XLFe.ODecl (fam a))
    | XLFa.FDecl k -> kont nlfs (XLFe.FDecl (kind k))

(* ... and back *)

let rec from_obj = function
  | XLFe.OLam(x,a,t) -> XLFa.OLam(x, from_fam a, from_obj t)
  | XLFe.OHead (h,l,a) -> XLFa.OHead(from_ohead h, from_args l, from_fhead a)

and from_args l = List.map (fun (x,t) -> x, from_obj t) l

and from_ohead = function
  | XLFe.HVar x -> XLFa.HVar x
  | XLFe.HConst c -> XLFa.HConst c
  | XLFe.HApp t -> XLFa.HApp (from_obj t)
  | XLFe.HMeta _ -> assert false

and from_fhead = function
  | XLFe.FConst (c,l) -> XLFa.FConst(c,from_args l, XLFa.KType)

and from_fam = function
  | XLFe.FProd(x,a,b) -> XLFa.FProd(x, from_fam a, from_fam b)
  | XLFe.FHead h -> from_fhead h

let rec from_kind = function
  | XLFe.KProd(x,a,k) -> XLFa.KProd(x,from_fam a,from_kind k)
  | XLFe.KType -> XLFa.KType
 
let rec from_sign s = 
  List.map
    (function
       | c, XLFe.ODecl a -> c, XLFa.ODecl(from_fam a)
       | c, XLFe.FDecl k -> c, XLFa.FDecl(from_kind k)
    ) s
