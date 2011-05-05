open Name

module XLFw = struct

  module Env = struct
    type t =  Env of (XLFe.obj * t) Varmap.t
    let add k v (Env e) = Env (Varmap.add k v e)
    let find k (Env e) = Varmap.find k e
    let empty = Env Varmap.empty
  end

  type ohead =
    | HVar of variable
    | HConst of constant

  and obj =
    | OClos of variable * XLFe.fam * XLFe.obj * Env.t
    | OMeta of definition * XLFe.fhead
    | OHead of ohead * XLFe.args * XLFe.fhead * Env.t (* this env applies to the arguments only *)
    | OBox of obj * variable * (variable * obj) list

end

module XLFe_XLFw = struct

  let rec obj' e : XLFe.obj -> XLFw.obj = function
    | XLFe.OLam(x,a,t) -> XLFw.OClos(x, a, t, e)
    | XLFe.OHead(XLFe.HVar x,l,a) -> 
	begin
	  try let (t,f) = XLFw.Env.find x e in
	      red e f (t,List.rev l)        (* TODO: variable capture in u *)
	  with Not_found -> XLFw.OHead(XLFw.HVar x, l, a, e)
	end
    | XLFe.OHead(XLFe.HConst c,l,a) -> XLFw.OHead(XLFw.HConst c, l, a, e)
    | XLFe.OHead(XLFe.HApp t,l,a) -> red e e (t, List.rev l)
    | XLFe.OMeta (x,a) -> XLFw.OMeta(x, a)
    | XLFe.OBox(t,p,s) -> XLFw.OBox(obj e t,p,List.map (fun x, t -> x, obj e t) s)

  and red' e f = function
    | XLFe.OHead(h,l,a), [] -> obj f (XLFe.OHead(h,l,a))
    | XLFe.OLam(x,a,t), (y,u) :: l ->
      (* Format.printf "red %a %a@." Name.Pp.variable x Name.Pp.variable y; *)
      (* assert(x=y); *)    (* Pas forcément: la substitution ne préserve pas des noms valides *)
      red e (XLFw.Env.add x (u,e) f) (t,l)
    | XLFe.OHead _, _ :: _ -> assert false
    | XLFe.OLam _, [] -> assert false
    | _ -> assert false		 (* head, u::l -> head is not a function (eta)
				  * lam, [] -> a function is always totally applied (eta)
				  * meta|box, _ -> no applied meta/box TODO error *)

  and obj e t =
    Util.if_debug (fun () -> Format.printf "@[<v 2>{ O %a@," XLFa.Pp.obj (XLFa_XLFe.from_obj t));
    let t = obj' e t in
    Util.if_debug (fun () -> Format.printf "@]} O@,");
    t

  and red e f (t,l) =
    Util.if_debug (fun () -> Format.printf "@[<v 2>{ R < %a | %a >@,"
      XLFa.Pp.obj (XLFa_XLFe.from_obj t)
      XLFa.Pp.args (XLFa_XLFe.from_args l)
    );
    let t = red' e f (t,l) in
    Util.if_debug (fun () -> Format.printf "@]} R@,");
    t

end

module rec XLFw_XLFn : sig
  val obj : XLFw.obj -> XLFn.obj
end = struct

  let rec obj : XLFw.obj -> XLFn.obj = function
    | XLFw.OMeta(x,a) -> XLFn.OMeta(x, XLFe_XLFn.fhead XLFw.Env.empty a)
    | XLFw.OHead(h,l,a,f) -> XLFn.OHead(ohead h, XLFe_XLFn.args f l, XLFe_XLFn.fhead f a)
    | XLFw.OClos(x,a,t,f) -> XLFn.OLam(x, XLFe_XLFn.fam a, obj (XLFe_XLFw.obj f t))
    | XLFw.OBox (t,p,s) -> XLFn.OBox(obj t, p, List.map (fun x, t -> x, obj t) s)

  and ohead = function
    | XLFw.HVar x -> XLFn.HVar x
    | XLFw.HConst c -> XLFn.HConst c

end

and XLFe_XLFn : sig
  val obj : XLFw.Env.t -> XLFe.obj -> XLFn.obj
  val args : XLFw.Env.t -> XLFe.args -> XLFn.args
  val fhead : XLFw.Env.t -> XLFe.fhead -> XLFn.fhead
  val fam : XLFe.fam -> XLFn.fam
  val kind : XLFe.kind -> XLFn.kind
end = struct
  let obj e t = XLFw_XLFn.obj (XLFe_XLFw.obj e t)
  let args e l = List.map (fun x, t -> x, obj e t) l

  let fhead e = function
    | XLFe.FConst(c,l) -> XLFn.FConst(c, args e l)

  let rec fam = function
    | XLFe.FProd(x,a,b) -> XLFn.FProd(x, fam a, fam b)
    | XLFe.FHead h -> XLFn.FHead (fhead XLFw.Env.empty h)

  let rec kind = function
    | XLFe.KType -> XLFn.KType
    | XLFe.KProd(x,a,k) -> XLFn.KProd(x, fam a, kind k)

end

let obj t = XLFe_XLFn.obj XLFw.Env.empty t
let fam a = XLFe_XLFn.fam a
let kind k = XLFe_XLFn.kind k
let entry kont nlfs = function
  | XLFe.ODecl a -> kont nlfs (XLFn.ODecl (fam a))
  | XLFe.FDecl k -> kont nlfs (XLFn.FDecl (kind k))

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
