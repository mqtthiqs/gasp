open NLF
open Util
open LF

let reify_fam t =
  let t = 
  ( XLFn_NLF.from_fam // XLFe_XLFn.from_fam // XLFa_XLFe.from_fam // XLF_XLFa.from_fam // 
      LF_XLF.from_fam) t
  in Util.if_debug (fun () -> Format.printf "** reified: %a@." SLF.Pp.term (SLF_LF.from_fam t));
  t
let reify_kind t =
  let t =
  ( XLFn_NLF.from_kind // XLFe_XLFn.from_kind // XLFa_XLFe.from_kind // XLF_XLFa.from_kind // 
      LF_XLF.from_kind) t
  in Util.if_debug (fun () -> Format.printf "** reified: %a@." SLF.Pp.term (SLF_LF.from_kind t));
  t

module Subst = struct
  module M = Map.Make(struct type t = string let compare = String.compare end)
  type t = obj M.t
  let find = M.find
  let add x t s = M.add x t s
  let add_name x t s = match x with Name.Anonymous -> s | Name.Named x -> add x t s
  let empty = M.empty
  let pp fmt subst = M.iter 
    (fun x t -> Format.fprintf fmt "[%s = %a]" x SLF.Pp.term (SLF_LF.from_obj t)
    ) subst
  let remove x t = M.remove x t
end
type subst = Subst.t

module Ren = struct
  let fresh =
    let i = ref 0 in
    fun () -> incr i; "_"^string_of_int !i

  let rec rename_fam x y = function		(* y must be fresh *)
    | FProd(z,a,b) -> 
	if z=Name.Named x then FProd(z,rename_fam x y a,b) 
	else FProd(z,rename_fam x y a, rename_fam x y b)
    | FApp(a,t) -> FApp(rename_fam x y a, rename_obj x y t)
    | FConst c -> FConst c
  and rename_obj x y = function
    | OMeta i -> OMeta i
    | OConst c -> OConst c
    | OVar z -> if z=x then OVar y else OVar z
    | OApp(t,u) -> OApp(rename_obj x y t, rename_obj x y u)
    | OLam(z,a,t) -> 
	if z=Name.Named x then OLam(z, rename_fam x y a, t)
	else OLam(z, rename_fam x y a, rename_obj x y t)
  let rec rename_kind x y = function
    | KProd(z,a,k) -> 
	if z=Name.Named x then KProd(z,rename_fam x y a,k)
	else KProd(z,rename_fam x y a, rename_kind x y k)
    | KType -> KType

  let map_name f x y t = match x with 
    | Name.Named x -> f x y t 
    | Name.Anonymous -> t

  let refresh_obj x t = let y = fresh() in y, map_name rename_obj x y t
  let refresh2_obj x y t u = let z = fresh() in z, map_name rename_obj x z t, map_name rename_obj y z u
  let refresh2_fam x y t u = let z = fresh() in z, map_name rename_fam x z t, map_name rename_fam y z u
  let refresh2_kind x y t u = let z = fresh() in z, map_name rename_kind x z t, map_name rename_kind y z u
end

(* Conversion *)

module State = struct
  type stack = obj list
  type t = subst * stack * obj
  let rec unwind (e,s,t) = match s with
    | [] -> t
    | a :: s -> OApp (unwind (e,s,t), a)
  let stack_pp fmt s = List.iter (fun t -> Format.fprintf fmt "%a " SLF.Pp.term (SLF_LF.from_obj t)) s
  let pp fmt (e,s,t:t) = Format.fprintf fmt "%a {%a} %a" SLF.Pp.term (SLF_LF.from_obj t)  stack_pp s Subst.pp e
end

let rec whd = function
  | env, stack, OApp (t,u) -> whd (env,(u :: stack),t)
  | env, a :: stack, OLam (x,_,t) -> 
      let x, t = Ren.refresh_obj x t in
      whd ((Subst.add x a env),stack,t)
  | env, stack, OVar x -> 
      (try whd (Subst.remove x env, stack, Subst.find x env)
       with Not_found -> env, stack, OVar x)
  | env, stack, t -> env, stack, t

exception Stack_not_convertible

let rec conv_stack (e1,s1) (e2,s2) =
  match s1,s2 with
    | t1 :: s1, t2 :: s2 -> 
	conv_obj (e1,s1,t1) (e2,s2,t2);
	conv_stack (e1,s1) (e2,s2)
    | [], [] -> ()
    | _ -> raise Stack_not_convertible

and conv_obj' st1 st2 = 
  let st1, st2 = whd st1, whd st2 in
  match st1, st2 with

    | (e1, [], OLam(x1,_,t1)), (e2, [], OLam(x2,_,t2)) -> 
	let _, t1, t2 = Ren.refresh2_obj x1 x2 t1 t2 in
	conv_obj (e1,[],t1) (e2,[],t2)

    | (e1, [], OLam(x1,_,t1)), (e2, s2, t2) ->
	let x, t1 = Ren.refresh_obj x1 t1 in
	conv_obj (e1, [], t1) (e2,OVar x::s2, t2)

    | (e1, s1, t1), (e2, [], OLam(x2,_,t2)) ->
	let x, t2 = Ren.refresh_obj x2 t2 in
	conv_obj (e1,OVar x::s1, t1) (e2, [], t2)

    | (e1, s1, OVar x1), (e2, s2, OVar x2) when x1=x2 ->
	(try conv_stack (e1,s1) (e2,s2)
	 with Stack_not_convertible ->
	   Errors.not_convertible (SLF_LF.from_obj (State.unwind st1)) (SLF_LF.from_obj (State.unwind st2)))

    | (e1, s1, OConst x1), (e2, s2, OConst x2) when x1=x2 ->
	(try conv_stack (e1,s1) (e2,s2)
	 with Stack_not_convertible ->
	   Errors.not_convertible (SLF_LF.from_obj (State.unwind st1)) (SLF_LF.from_obj (State.unwind st2)))

    | (_, _::_, OLam _), _ | _, (_, _::_, OLam _) -> 
	assert false (* whd would have eaten it *)

    | (_, _, OMeta _), (_, _, OMeta _) -> assert false (* not implemented *)

    | _ -> Errors.not_convertible (SLF_LF.from_obj (State.unwind st1)) (SLF_LF.from_obj (State.unwind st2))

and conv_obj st1 st2 =
  Util.if_debug (fun () -> Format.printf "* CO %a == %a@," State.pp st1 State.pp st2);
  conv_obj' st1 st2

let rec conv_fam e1 e2 a b =
  match a, b with
    | FConst c, FConst d when c = d -> ()
    | FProd(x1,a1,b1), FProd(x2,a2,b2) ->
	conv_fam e1 e2 a1 a2;
	let _, b1, b2 = Ren.refresh2_fam x1 x2 b1 b2 in
	conv_fam e1 e2 b1 b2
    | FApp (a,t), FApp(b,u) -> conv_fam e1 e2 a b; conv_obj (e1,[],t) (e2,[],u)
    | _ -> Errors.not_convertible (SLF_LF.from_fam a) (SLF_LF.from_fam b)

let rec conv_kind e1 e2 k l = 
  match k,l with
    | KType, KType -> ()
    | KProd(x1,a1,k1), KProd(x2,a2,k2) -> 
	conv_fam e1 e2 a1 a2;
	let _, k1, k2 = Ren.refresh2_kind x1 x2 k1 k2 in
	conv_kind e1 e2 k1 k2
    | _ -> assert false

let conv_obj e1 e2 t u = 
  Util.if_debug (fun () -> Format.printf "* CO %a %a == %a %a@," SLF.Pp.term (SLF_LF.from_obj t) Subst.pp e1 SLF.Pp.term (SLF_LF.from_obj u) Subst.pp e2);
  conv_obj (e1,[],t) (e2,[],t)
let conv_fam e1 e2 a b = 
  Util.if_debug (fun () -> Format.printf "* CF %a %a == %a %a@," SLF.Pp.term (SLF_LF.from_fam a) Subst.pp e1 SLF.Pp.term (SLF_LF.from_fam b) Subst.pp e2);
  conv_fam e1 e2 a b
let conv_kind e1 e2 k l = 
  Util.if_debug (fun () -> Format.printf "* CK %a %a == %a %a@," SLF.Pp.term (SLF_LF.from_kind k) Subst.pp e1 SLF.Pp.term (SLF_LF.from_kind l)Subst.pp e2);
  conv_kind e1 e2 k l

(* Typing *)

let rec obj' sign env subst t : fam * subst = 
  match t with
  | OConst c -> begin match NLFSign.find c sign with
      | NLF.FDecl k -> Errors.not_an_obj (SLF_LF.from_obj t)
      | NLF.ODecl a -> reify_fam a, subst
    end
  | OVar x -> List.assoc x env, subst
  | OLam (x,a,t) -> 
      let k, _ = fam sign env subst a in
      conv_kind subst Subst.empty k KType;
      let b, _ = obj sign ((Name.variable_for x,a)::env) subst t in
      FProd(x,a,b), subst
  | OApp (t,u) ->
      begin match obj sign env subst t with
	| FProd(x,a,b), e1 ->
	    (let a', e2 = obj sign env subst u in
	    conv_fam e1 e2 a a');
	    b, Subst.add_name x u e1
	| _ -> Errors.bad_application (SLF_LF.from_obj t)
      end
  | OMeta _ -> assert false

and fam' sign env subst = function
  | FProd(x,a,b) -> 
      let k, _ = fam sign env subst a in 
      conv_kind subst Subst.empty k KType; 
      fam sign ((Name.variable_for x,a)::env) subst b
  | FConst c -> begin match NLFSign.find c sign with
      | NLF.FDecl k -> reify_kind k, subst
      | NLF.ODecl _ -> assert false
    end
  | FApp (a,t) -> 
      match fam sign env subst a with
	| KProd(x,a,k), e1 -> 
	    (let a', e2 = obj sign env subst t in
	     conv_fam e1 e2 a a');
	    k, Subst.add_name x t e1
	| KType, _ -> assert false
	    
and kind' sign env subst k =
  match k with
    | KType -> ()
    | KProd(x,a,k) -> 
	let k, subst = fam sign env subst a in
	conv_kind subst Subst.empty k KType;
	kind sign ((Name.variable_for x,a)::env) subst k

and obj sign env subst t =
  Util.if_debug (fun () -> Format.printf "@[<v 2>{ O %a ⊢ %a@," Subst.pp subst SLF.Pp.term (SLF_LF.from_obj t));
  let a, subst' = obj' sign env subst t in
    Util.if_debug (fun () -> Format.printf "@]} O %a : %a => %a@," SLF.Pp.term (SLF_LF.from_obj t) SLF.Pp.term (SLF_LF.from_fam a) Subst.pp subst');
  a, subst'
and fam sign env subst a =
  Util.if_debug (fun () -> Format.printf "@[<v 2>{ F %a ⊢ %a@," Subst.pp subst SLF.Pp.term (SLF_LF.from_fam a));
  let k, subst' = fam' sign env subst a in
  Util.if_debug (fun () -> Format.printf "@]} F %a : %a => %a@," SLF.Pp.term (SLF_LF.from_fam a) SLF.Pp.term (SLF_LF.from_kind k) Subst.pp subst');
  k, subst'
and kind sign env subst k =
  Util.if_debug (fun () -> Format.printf "@[<v 2>{ K %a ⊢ %a@," Subst.pp subst SLF.Pp.term (SLF_LF.from_kind k));
  let () = kind' sign env subst k in
  Util.if_debug (fun () -> Format.printf "@]} K %a : ok.@," SLF.Pp.term (SLF_LF.from_kind k))

