open NLF
open Util
open LF

let reify_obj t =
  (XLFe_NLF.from_obj // XLFa_XLFe.from_obj // XLF_XLFa.from_obj // 
     LF_XLF.from_obj) t
let reify_fam t =
  (XLFe_NLF.from_fam // XLFa_XLFe.from_fam // XLF_XLFa.from_fam // 
     LF_XLF.from_fam) t
let reify_kind t =
  (XLFe_NLF.from_kind // XLFa_XLFe.from_kind // XLF_XLFa.from_kind // 
     LF_XLF.from_kind) t

module Subst = struct
  module M = Map.Make(struct type t = string let compare = String.compare end)
  type t = obj M.t
  let find = M.find
  let add x t s = M.add x t s
  let add_name x t s = match x with Name.Anonymous -> s | Name.Named x -> add x t s
  let empty = M.empty
  let pp fmt subst = M.iter 
    (fun x t -> Format.fprintf fmt "[%s = %a]" x SLF_pp.term (SLF_LF.from_obj t)
    ) subst
  let remove x t = M.remove x t
end
type subst = Subst.t

module Ren = struct
  module M = Map.Make(struct type t = string let compare = String.compare end)
  type t = string M.t
  let find x r = try M.find x r with Not_found -> x
  let add x y r = match x,y with
    | Name.Named x, Name.Named y -> M.add x y r
    | _ -> r
  let pp fmt subst = M.iter (Format.fprintf fmt "(%s/%s)") subst
  let empty = M.empty
end

(* Conversion *)

type stack = obj list
type state = subst * stack * obj

let rec unwind (e,s,t) = match s with
  | [] -> t
  | a :: s -> OApp (unwind (e,s,t), a)

let rec whd ren : state -> state = function
  | env, stack, OApp (t,u) -> whd ren (env,(u :: stack),t)
  | env, a :: stack, OLam (x,_,t) -> whd ren ((Subst.add_name x a env),stack,t)
  | env, stack, OVar x -> 
      (try whd ren (Subst.remove x env, stack, Subst.find x env)
       with Not_found -> env, stack, OVar x)
  | env, stack, t -> env, stack, t

exception Stack_not_convertible
  
let rec conv_stack ren (e1,s1) (e2,s2) =
  match s1,s2 with
    | t1 :: s1, t2 :: s2 -> 
	conv_obj ren (e1,s1,t1) (e2,s2,t2);
	conv_stack ren (e1,s1) (e2,s2)
    | [], [] -> ()
    | _ -> raise Stack_not_convertible

and conv_obj ren (st1 : state) (st2 : state) = 
  let st1, st2 = whd ren st1, whd ren st2 in
  match st1, st2 with

    | (e1, [], OLam(x1,_,t1)), (e2, [], OLam(x2,_,t2)) -> 
	conv_obj (Ren.add x1 x2 ren) (e1,[],t1) (e2,[],t2)

    | (e1, s1, OVar x1), (e2, s2, OVar x2) when Ren.find x1 ren = x2 ->
	(try conv_stack ren (e1,s1) (e2,s2)
	 with Stack_not_convertible ->
	   Errors.not_convertible (SLF_LF.from_obj (unwind st1)) (SLF_LF.from_obj (unwind st2)))

    | (e1, s1, OConst x1), (e2, s2, OConst x2) when x1 = x2 ->
	(try conv_stack ren (e1,s1) (e2,s2)
	 with Stack_not_convertible ->
	   Errors.not_convertible (SLF_LF.from_obj (unwind st1)) (SLF_LF.from_obj (unwind st2)))

    | (_, _::_, OLam _), _ | _, (_, _::_, OLam _) -> 
	assert false (* whd would have eaten it *)

    | (_, _, OMeta _), (_, _, OMeta _) -> assert false (* not implemented *)

    | _ -> Errors.not_convertible (SLF_LF.from_obj (unwind st1)) (SLF_LF.from_obj (unwind st2))

let rec conv_fam e1 e2 ren a b =
  match a, b with
    | FConst c, FConst d when c = d -> ()
    | FProd(x,a,b), FProd(y,c,d) ->
	conv_fam e1 e2 ren a c;
	conv_fam e1 e2 (Ren.add x y ren) b d
    | FApp (a,t), FApp(b,u) -> conv_fam e1 e2 ren a b; conv_obj ren (e1,[],t) (e2,[],u)
    | _ -> Errors.not_convertible (SLF_LF.from_fam a) (SLF_LF.from_fam b)

let rec conv_kind e1 e2 ren k l = 
  match k,l with
    | KType, KType -> ()
    | KProd(x,a,k), KProd(y,b,l) -> 
	conv_fam e1 e2 ren a b;
	conv_kind e1 e2 (Ren.add x y ren) k l
    | _ -> assert false

let conv_obj e1 e2 t u = 
  Util.if_debug (fun () -> Format.printf "= CO %a %a == %a %a@." SLF_pp.term (SLF_LF.from_obj t) Subst.pp e1 SLF_pp.term (SLF_LF.from_obj u) Subst.pp e2);
  conv_obj Ren.empty (e1,[],t) (e2,[],t)
let conv_fam e1 e2 a b = 
  Util.if_debug (fun () -> Format.printf "= CF %a %a == %a %a@." SLF_pp.term (SLF_LF.from_fam a) Subst.pp e1 SLF_pp.term (SLF_LF.from_fam b) Subst.pp e2);
  conv_fam e1 e2 Ren.empty a b
let conv_kind e1 e2 k l = 
  Util.if_debug (fun () -> Format.printf "= CK %a %a == %a %a@." SLF_pp.term (SLF_LF.from_kind k) Subst.pp e1 SLF_pp.term (SLF_LF.from_kind l)Subst.pp e2);
  conv_kind e1 e2 Ren.empty k l

(* Typing *)

let rec obj' sign env subst t : fam * subst = 
  match t with
  | OConst c -> begin match NLFSign.find sign c with
      | NLFSign.FDecl k -> Errors.not_an_obj (SLF_LF.from_obj t)
      | NLFSign.ODecl a -> reify_fam a, subst
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
  | FConst c -> begin match NLFSign.find sign c with
      | NLFSign.FDecl k -> reify_kind k, subst
      | NLFSign.ODecl _ -> assert false
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
  Util.if_debug (fun () -> Format.printf "[ O %a ⊢ %a@." Subst.pp subst SLF_pp.term (SLF_LF.from_obj t));
  let a, subst' = obj' sign env subst t in
    Util.if_debug (fun () -> Format.printf "] O %a : %a => %a@." SLF_pp.term (SLF_LF.from_obj t) SLF_pp.term (SLF_LF.from_fam a) Subst.pp subst');
  a, subst'
and fam sign env subst a =
  Util.if_debug (fun () -> Format.printf "[ F %a ⊢ %a@." Subst.pp subst SLF_pp.term (SLF_LF.from_fam a));
  let k, subst' = fam' sign env subst a in
  Util.if_debug (fun () -> Format.printf "] F %a : %a => %a@." SLF_pp.term (SLF_LF.from_fam a) SLF_pp.term (SLF_LF.from_kind k) Subst.pp subst');
  k, subst'
and kind sign env subst k =
    Util.if_debug (fun () -> Format.printf "[ K %a ⊢ %a@." Subst.pp subst SLF_pp.term (SLF_LF.from_kind k));
  let () = kind' sign env subst k in
    Util.if_debug (fun () -> Format.printf "] K %a : ok.@." SLF_pp.term (SLF_LF.from_kind k))
