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
    | HMeta of NLF.variable
    | HConst of constant

  and obj =
    | OClos of variable * fam * XLFe.obj * Env.t
    | OHead of ohead * args * fhead

  and args = (variable * obj) list
end

module XLFe_XLFw = struct

  let rec obj e : XLFe.obj -> XLFw.obj = function
    | XLFe.OLam(x,a,t) -> XLFw.OClos(x, fam e a, t, e)
    | XLFe.OHead(XLFe.HVar x,l,a) -> 
	begin
	  try let (u,f) = XLFw.Env.find x e in
	  obj f (XLFe.OHead(XLFe.HApp u, l, a))	(* TODO: variable capture in u *)
	  with Not_found -> XLFw.OHead(XLFw.HVar x, args e l, fhead e a)
	end
    | XLFe.OHead(XLFe.HMeta x,l,a) -> assert false	(* TODO *)
    | XLFe.OHead(XLFe.HConst c,l,a) -> XLFw.OHead(XLFw.HConst c, args e l, fhead e a)
    | XLFe.OHead(XLFe.HApp t,l,a) -> 
	match obj e t, l with
	  | XLFw.OClos (y,a,t,f), (x,u)::l -> 
	      assert (x=y);
	      obj (XLFw.Env.add x (u,e) f) t
	  | XLFw.OClos _, [] -> assert false (* OK b/c of the App invariant *)
	  | XLFw.OHead (h,l',_), _ -> 
	      XLFw.OHead (h, l' @ args e l, fhead e a)
		
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

  let rec obj e : XLFw.obj -> XLFn.obj = function
    | XLFw.OHead (h,l,a) -> XLFn.OHead(ohead e h, args e l, fhead e a)
    | XLFw.OClos(x,a,t,f) -> XLFn.OLam(x, fam e a, obj f (XLFe_XLFw.obj f t)) (* TODO: Quand utiliser e ou f ? *)

  and ohead e = function
    | XLFw.HVar x -> XLFn.HVar x
    | XLFw.HConst c -> XLFn.HConst c
    | XLFw.HMeta x -> XLFn.HMeta x	(* TODO *)

  and args e l = List.map (fun (x,t) -> x, obj e t) l

  and fhead e = function
    | XLFw.FConst(c,l) -> XLFn.FConst(c, args e l)

  and fam e = function
    | XLFw.FProd(x,a,b) -> XLFn.FProd(x, fam e a, fam e b)
    | XLFw.FHead h -> XLFn.FHead (fhead e h)

  let rec kind = function
    | XLFw.KType -> XLFn.KType
    | XLFw.KProd(x,a,k) -> XLFn.KProd(x, fam XLFw.Env.empty a, kind k)

end

let obj t = XLFw_XLFn.obj XLFw.Env.empty (XLFe_XLFw.obj XLFw.Env.empty t)
let fam a = XLFw_XLFn.fam XLFw.Env.empty (XLFe_XLFw.fam XLFw.Env.empty a)
let kind k = XLFw_XLFn.kind (XLFe_XLFw.kind k)

let entry kont nlfs = function 
    | XLFe.ODecl a -> kont nlfs (XLFn.ODecl (fam a))
    | XLFe.FDecl k -> kont nlfs (XLFn.FDecl (kind k))
