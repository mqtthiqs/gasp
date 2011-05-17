open Name

include types of mli with

module NLFSubst = struct
  type key = variable
  type value = NLF.vhead * NLF.args * constant * NLF.args
  type t = value Varmap.t
  let add x e m = Varmap.add x e m
  let find x m = Varmap.find x m
  let fold f m acc = Varmap.fold f m acc
  let is_empty t = Varmap.is_empty t
  let empty = Varmap.empty
end

and module NLFSign = struct
  type key = constant
  type value = NLF.entry
  type t = value Constmap.t
      
  let add x e env = Constmap.add x e env
  let find x env = Constmap.find x env
  let fold f env acc = Constmap.fold f env acc
  let empty = Constmap.empty
  let is_empty = Constmap.is_empty
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
    | H of vhead
    | B of subst
    | A of args
    | V of value
    | S of NLFSign.t

  let ent_prec = function
      _ -> 10

  module S = NLFSubst

  let pp pp : entity printing_fun =
    let pr_fhead fmt (c, fargs) : unit =
      if fargs = [] then constant fmt c else
	fprintf fmt "@[%a@ %a@]" constant c (pp (<=)) (A fargs) in
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
    | H(XLF.HConst c) -> constant fmt c
    | A a -> pr_list pr_spc (fun fmt (x,a) -> fprintf fmt "@[%a@ %a@]" variable x (pp (<=)) (V a)) fmt a
    | B b -> NLFSubst.fold (fun x (h,a,c,b) () -> fprintf fmt "@[[%a@ =@ %a@ %a@ :@ %a@ %a]@]@," variable x (pp (<=)) (H h) (pp (<=)) (A a) constant c (pp (<=)) (A b)) b ()
    | S s -> NLFSign.fold
      (fun c e () ->
  	match e with
  	  | NLF.ODecl a -> fprintf fmt "@[%a@ :@ %a@]@," constant c (pp (<=)) (F a)
  	  | NLF.FDecl k -> fprintf fmt "@[%a@ :@ %a@]@," constant c (pp (<=)) (K k)
      ) s ()
    | V(VHead (h,c,l)) -> fprintf fmt "@[%a@ :@ %a@ %a@]" (pp (<=)) (H h) constant c (pp (<=)) (A l)
    | V(VLam (x,a,t)) -> fprintf fmt "@[λ%a@ :@ %a.@ %a@]" variable x (pp (<=)) (F a) (pp (<=)) (O t)

  let sign fmt s = pr_paren pp ent_prec 100 (<=) fmt (S s)
  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
  let fam fmt s = pr_paren pp ent_prec 100 (<=) fmt (F s)
  let kind fmt s = pr_paren pp ent_prec 100 (<=) fmt (K s)
  let entry fmt = function
    | NLF.FDecl k -> pr_paren pp ent_prec 100 (<=) fmt (K k)
    | NLF.ODecl a -> pr_paren pp ent_prec 100 (<=) fmt (F a)

end

let lift_def x = function
  | NLF.Obj(subst, _) ->
      let _, _, c, fargs = NLFSubst.find x subst in
      NLF.FHead(NLFSubst.empty, c, fargs)

let to_def = function
  | NLF.Obj(subst, NLF.VHead(XLF.HVar x, _, _)) ->
    NLFSubst.find x subst
  | _ -> assert false			(* TODO erreur *)

let go term p u = match term, p with
  | NLF.Obj(_, NLF.VLam (x, a, NLF.Obj(s, v))), None -> (* TODO que faire de _? *)
    NLF.Obj(NLFSubst.add x u s, v)
  | NLF.Obj(_, NLF.VHead _), None -> assert false (* TODO erreur *)
  | NLF.Obj(s, _), Some (x, n) ->
    let h, l, a, m = NLFSubst.find x s in
    match List.nth l n with
      | _, NLF.VLam (x, a, NLF.Obj(s, v)) ->
	NLF.Obj(NLFSubst.add x u s, v)
      | _, NLF.VHead _ -> assert false	(* TODO error *)


let bidon =
  let x = Name.gen_variable() in
  let s = NLFSubst.add x
    (XLF.HConst(Name.mk_constant "bidon"), [], Name.mk_constant "Bidon", [])
    NLFSubst.empty in
  NLF.Obj(s, NLF.VHead(XLF.HVar x, Name.mk_constant "Bidon", []))
