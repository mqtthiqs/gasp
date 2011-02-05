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
  
let rec conv_obj substl substr ren t u = 
  match t, u with
    | OMeta _, OMeta _ -> assert false
    | OConst x, OConst y when x=y -> ()
    | OVar x, _ -> 
	let x = Ren.find x ren in
	begin try conv_obj (Subst.remove x substl) substr ren (Subst.find x substl) u
	with Not_found -> 
	  match u with OVar y when x = y -> () | _ -> assert false
	end
    | _, OVar y ->
	begin try conv_obj substl (Subst.remove y substr) ren t (Subst.find y substr)
	with Not_found ->
	  match t with OVar x when x = y -> () | _ ->  assert false
	end
    | OApp(t,u), OApp(t',u') ->
	conv_obj substl substr ren t t';
	conv_obj substl substr ren u u'
    | _ -> assert false

let rec conv_fam substl substr ren a b =
  match a, b with
    | FConst c, FConst d when c = d -> ()
    | FProd(x,a,b), FProd(y,c,d) ->
	conv_fam substl substr ren a c;
	conv_fam substl substr (Ren.add x y ren) b d
    | FApp (a,t), FApp(b,u) -> conv_fam substl substr ren a b; conv_obj substl substr ren t u
    | _ -> Errors.not_convertible_fam (SLF_LF.from_fam a) (SLF_LF.from_fam b)

let rec conv_kind substl substr ren k l = 
  match k,l with
    | KType, KType -> ()
    | KProd(x,a,k), KProd(y,b,l) -> 
	conv_fam substl substr ren a b;
	conv_kind substl substr (Ren.add x y ren) k l
    | _ -> assert false

let conv_obj substl substr t u = 
  Util.if_debug (fun () -> Format.printf "= CO %a %a == %a %a@." SLF_pp.term (SLF_LF.from_obj t) Subst.pp substl SLF_pp.term (SLF_LF.from_obj u) Subst.pp substr);
  conv_obj substl substr Ren.empty t u
let conv_fam substl substr a b = 
  Util.if_debug (fun () -> Format.printf "= CF %a %a == %a %a@." SLF_pp.term (SLF_LF.from_fam a) Subst.pp substl SLF_pp.term (SLF_LF.from_fam b) Subst.pp substr);
  conv_fam substl substr Ren.empty a b
let conv_kind substl substr k l = 
  Util.if_debug (fun () -> Format.printf "= CK %a %a == %a %a@." SLF_pp.term (SLF_LF.from_kind k) Subst.pp substl SLF_pp.term (SLF_LF.from_kind l)Subst.pp substr);
  conv_kind substl substr Ren.empty k l

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
	| FProd(x,a,b), substl ->
	    (let a', substr = obj sign env subst u in
	    conv_fam substl substr a a');
	    b, Subst.add_name x u substl
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
	| KProd(x,a,k), substl -> 
	    (let a', substr = obj sign env subst t in
	     conv_fam substl substr a a');
	    k, Subst.add_name x t substl
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
    Util.if_debug (fun () -> Format.printf "] O %a : %a => %a@." SLF_pp.term (SLF_LF.from_obj t) SLF_pp.term (SLF_LF.from_fam a) Subst.pp subst);
  a, subst'
and fam sign env subst a =
  Util.if_debug (fun () -> Format.printf "[ F %a ⊢ %a@." Subst.pp subst SLF_pp.term (SLF_LF.from_fam a));
  let k, subst' = fam' sign env subst a in
  Util.if_debug (fun () -> Format.printf "] F %a : %a => %a@." SLF_pp.term (SLF_LF.from_fam a) SLF_pp.term (SLF_LF.from_kind k) Subst.pp subst);
  k, subst'
and kind sign env subst k =
    Util.if_debug (fun () -> Format.printf "[ K %a ⊢ %a@." Subst.pp subst SLF_pp.term (SLF_LF.from_kind k));
  let () = kind' sign env subst k in
    Util.if_debug (fun () -> Format.printf "] K %a : ok.@." SLF_pp.term (SLF_LF.from_kind k))
