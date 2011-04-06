open Name

module XLFw = struct

  module Env = struct
    type t =  Env of (XLFe.obj * t) Varmap.t
    let add k v (Env e) = Env (Varmap.add k v e)
    let find k (Env e) = Varmap.find k e
    let empty = Env Varmap.empty
  end

  type kind =
    | KType
    | KProd of variable * fam * kind

  and fhead =
    | FConst of constant * args

  and fam =
    | FProd of variable * fam * fam
    | FHead of fhead

  and ohead =
    | HVar of variable
    | HConst of constant

  and obj =
    | OClos of variable * fam * XLFe.obj * Env.t
    | OMeta of definition * fhead
    | OHead of ohead * args * fhead
    | OBox of obj * variable * args

  and args = (variable * obj) list
end

module XLFe_XLFw = struct

  let rec obj e : XLFe.obj -> XLFw.obj = function
    | XLFe.OLam(x,a,t) -> XLFw.OClos(x, fam e a, t, e)
    | XLFe.OHead(XLFe.HVar x,l,a) -> 
	begin
	  try let (t,f) = XLFw.Env.find x e in
	      red e t f l        (* TODO: variable capture in u *)
	  with Not_found -> XLFw.OHead(XLFw.HVar x, args e l, fhead e a)
	end
    | XLFe.OHead(XLFe.HConst c,l,a) -> XLFw.OHead(XLFw.HConst c, args e l, fhead e a)
    | XLFe.OHead(XLFe.HApp t,l,a) -> red e t e l
    | XLFe.OMeta (x,a) -> XLFw.OMeta(x, fhead e a)
    | XLFe.OBox(t,p,s) -> XLFw.OBox(obj e t,p,List.map (fun x, t -> x, obj e t) s)

  and red e t f = function
    | [] -> obj f t
    | (x,u) :: l ->
      begin match obj f t with
	| XLFw.OClos(y,a,v,g) ->
	  assert(x=y);
	  red e v (XLFw.Env.add x (u,e) g) l
	| XLFw.OHead _ -> assert false 	(* bc a head is not a function (eta) *)
	| XLFw.OMeta _ -> assert false	(* TODO error: applied meta forbidden *)
	| XLFw.OBox _ -> assert false	(* TODO error: applied box forbidden *)
      end

  and fhead e = function
    | XLFe.FConst(c,l) -> XLFw.FConst(c, args e l)

  and fam e = function
    | XLFe.FProd(x,a,b) -> XLFw.FProd(x, fam e a, fam e b)
    | XLFe.FHead(XLFe.FConst(c,l)) -> XLFw.FHead(XLFw.FConst(c, args e l))
	
  and args e l = List.map (fun (x,a) -> x, obj e a) l
    
  let rec kind = function
    | XLFe.KType -> XLFw.KType
    | XLFe.KProd(x,a,k) -> XLFw.KProd(x, fam XLFw.Env.empty a, kind k)
end

module XLFw_XLFn = struct

  let rec obj : XLFw.obj -> XLFn.obj = function
    | XLFw.OMeta(x,a) -> XLFn.OMeta(x, fhead a)
    | XLFw.OHead(h,l,a,f) -> XLFn.OHead(ohead h, args l, fhead a)
    | XLFw.OClos(x,a,t,f) -> XLFn.OLam(x, fam a, obj (XLFe_XLFw.obj f t)) (* TODO: Quand utiliser e ou f ? *)
    | XLFw.OBox (t,p,s) -> XLFn.OBox(obj t, p, List.map (fun x, t -> x, obj t) s)

  and ohead = function
    | XLFw.HVar x -> XLFn.HVar x
    | XLFw.HConst c -> XLFn.HConst c

  and args l = List.map (fun (x,t) -> x, obj t) l

  and fhead = function
    | XLFw.FConst(c,l) -> XLFn.FConst(c, args l)

  and fam = function
    | XLFw.FProd(x,a,b) -> XLFn.FProd(x, fam a, fam b)
    | XLFw.FHead h -> XLFn.FHead (fhead h)

  let rec kind = function
    | XLFw.KType -> XLFn.KType
    | XLFw.KProd(x,a,k) -> XLFn.KProd(x, fam a, kind k)

end

module XLFe_XLFn = struct
  let obj t = XLFw_XLFn.obj (XLFe_XLFw.obj XLFw.Env.empty t)
  let fam a = XLFw_XLFn.fam (XLFe_XLFw.fam XLFw.Env.empty a)
  let kind k = XLFw_XLFn.kind (XLFe_XLFw.kind k)

  let entry kont nlfs = function 
    | XLFe.ODecl a -> kont nlfs (XLFn.ODecl (fam a))
    | XLFe.FDecl k -> kont nlfs (XLFn.FDecl (kind k))
end

include XLFe_XLFn

(* ... and back: *)

let ohead = function
  | XLFn.HVar x -> XLFe.HVar x
  | XLFn.HConst c -> XLFe.HConst c

let rec from_obj = function
  | XLFn.OLam(x,a,t) -> XLFe.OLam(x, from_fam a, from_obj t)
  | XLFn.OHead(h,l,a) -> XLFe.OHead(ohead h, from_args l, from_fhead a)
  | XLFn.OMeta(x,a) -> XLFe.OMeta(x, from_fhead a)
  | XLFn.OBox(t,p,s) -> XLFe.OBox(from_obj t, p, List.map (fun x, t -> x, from_obj t) s)

and from_fhead = function XLFn.FConst (a,l) -> XLFe.FConst(a, from_args l)

and from_args l = List.map (fun (x,t) -> x, from_obj t) l

and from_fam = function
  | XLFn.FProd(x,a,b) -> XLFe.FProd(x, from_fam a, from_fam b)
  | XLFn.FHead h -> XLFe.FHead (from_fhead h)

let rec from_kind = function
  | XLFn.KType -> XLFe.KType
  | XLFn.KProd(x,a,k) -> XLFe.KProd(x, from_fam a, from_kind k)

let from_sign s = List.map 
  ( function 
      | x, XLFn.ODecl a -> x, XLFe.ODecl (from_fam a)
      | x, XLFn.FDecl k -> x, XLFe.FDecl (from_kind k)
  ) s
