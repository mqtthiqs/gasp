open Name

include types of mli with

module NLFSubst = Varmap

and module NLFSign = struct
  module FDecl = Fconstmap
  module ODecl = Oconstmap
  type t = NLF.kind FDecl.t * NLF.fam ODecl.t
  let fold f (fc,oc) acc = FDecl.fold (fun x k acc -> f (NLF.FDecl (x,k)) acc) fc
    (ODecl.fold (fun x a acc -> f (NLF.ODecl(x,a)) acc) oc acc)
  let empty = FDecl.empty, ODecl.empty
end

and module Pp = struct
  open Format
  open Print
  open Name.Pp
  open NLF

  type entity = 
    | K of kind
    | F of fam
    | O of obj
    | H of ohead
    | B of subst
    | A of args
    | V of value
    | S of NLFSign.t

  let ent_prec = function
      _ -> 10

  module S = NLFSubst

 let pp pp : entity printing_fun = assert false
    let pr_fhead fmt (c, fargs) : unit =
      if fargs = [] then fconst fmt c else
    	fprintf fmt "@[%a@ %a@]" fconst c (pp (<=)) (A fargs) in
    fun fmt (e:entity) -> match e with
    | K(KType) -> fprintf fmt "@[type@]"
    | K(KProd(x,a,k)) ->
      fprintf fmt "@[Π%a@ :@ %a. %a@]" variable x (pp (<=)) (F a) (pp (<=)) (K k)
    | F(FHead(s,c,fargs)) when S.is_empty s -> fprintf fmt "@[%a@]" pr_fhead (c, fargs)
    | F(FHead(s,c,fargs)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) pr_fhead (c, fargs)
    | F(FProd(x,a,b)) ->
      fprintf fmt "@[Π%a@ :@ %a. %a@]" variable x (pp (<=)) (F a) (pp (<=)) (F b)
    | O(Obj(s, v)) when S.is_empty s -> pp (<=) fmt (V v)
    | O(Obj(s, v)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) (pp (<=)) (V v)
    | H(XLF.HVar x) -> variable fmt x
    | H(XLF.HConst c) -> oconst fmt c
    | A a -> pr_list pr_spc (fun fmt a -> fprintf fmt "@[%a@]" (pp (<=)) (V a)) fmt a
    | B b -> NLFSubst.fold (fun x d () -> match d with
	| DApp (h,a,c,b) -> fprintf fmt "@[[%a@ =@ %a@ %a@ :@ %a@ %a]@]@," variable x (pp (<=)) (H h) (pp (<=)) (A a) fconst c (pp (<=)) (A b)
	| DHead (h,a) -> fprintf fmt "@[%a@ :@ %a@]" (pp (<=)) (H h) (pp (<=)) (F a)
    ) b ()
    | S s -> NLFSign.fold
      (fun e () ->
    	match e with
    	  | NLF.ODecl (c,a) -> fprintf fmt "@[%a@ :@ %a@]@," oconst c (pp (<=)) (F a)
    	  | NLF.FDecl (c,k) -> fprintf fmt "@[%a@ :@ %a@]@," fconst c (pp (<=)) (K k)
      ) s ()
    | V(VHead (h,c,l)) -> fprintf fmt "@[%a@ :@ %a@ %a@]" (pp (<=)) (H h) fconst c (pp (<=)) (A l)
    | V(VLam (x,a,t)) -> fprintf fmt "@[λ%a@ :@ %a.@ %a@]" variable x (pp (<=)) (F a) (pp (<=)) (O t)

  let sign fmt s = pr_paren pp ent_prec 100 (<=) fmt (S s)
  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
  let fam fmt s = pr_paren pp ent_prec 100 (<=) fmt (F s)
  let kind fmt s = pr_paren pp ent_prec 100 (<=) fmt (K s)
end

let lift_def x = function
  | NLF.Obj(subst, _) ->
      match NLFSubst.find x subst with
	| NLF.DApp (_, _, c, fargs) -> NLF.FHead(NLFSubst.empty, c, fargs)
	| NLF.DHead (_, a) -> a

let to_def = function
  | NLF.Obj(subst, NLF.VHead(XLF.HVar x, _, _)) ->
    NLFSubst.find x subst
  | _ -> assert false			(* TODO erreur *)

let go term p u = match term, p with
  | NLF.Obj(_, NLF.VLam (x, a, NLF.Obj(s, v))), None -> (* TODO que faire de _? *)
    NLF.Obj(NLFSubst.add x u s, v)
  | NLF.Obj(_, NLF.VHead _), None -> assert false (* TODO erreur *)
  | NLF.Obj(s, _), Some (x, n) ->
    match NLFSubst.find x s with
      | NLF.DHead (h, a) -> assert false (* TODO erreur *)
      | NLF.DApp (h, l, a, m) ->
	match List.nth l n with
	  | NLF.VLam (x, a, NLF.Obj(s, v)) ->
	    NLF.Obj(NLFSubst.add x u s, v)
	  | NLF.VHead _ -> assert false	(* TODO error *)

let bidon =
  let x = Name.gen_variable() in
  let s = NLFSubst.add x
    (NLF.DApp (XLF.HConst(Name.mk_oconst "bidon"), [], Name.mk_fconst "Bidon", []))
    NLFSubst.empty in
  NLF.Obj(s, NLF.VHead(XLF.HVar x, Name.mk_fconst "Bidon", []))
