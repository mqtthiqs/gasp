open Name

include types of mli with

module Pp = struct
  open Format
  open Print
  open Name.Pp

  type entity = 
    | O of obj
    | H of ohead
    | B of subst
    | A of args
    | V of value

  let ent_prec = function
      _ -> 10

  module S = Varmap

 let pp pp : entity printing_fun =
    fun fmt (e:entity) -> match e with
    | O(Obj(s, v)) when S.is_empty s -> pp (<=) fmt (V v)
    | O(Obj(s, v)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) (pp (<=)) (V v)
    | H(XLF.HVar x) -> variable fmt x
    | H(XLF.HConst c) -> oconst fmt c
    | A a ->
      let a = Varmap.fold (fun x v acc -> (x,v) :: acc) a [] in
      pr_list pr_spc (fun fmt (x, a) -> fprintf fmt "@[%a=%a@]" variable x (pp (<)) (V a)) fmt a
    | B b -> Varmap.fold (fun x d () -> match d with
	| DApp (h,a,c,b) -> fprintf fmt "@[%a@ =@ %a@ %a@ :@ %a@ %a, @]@," variable x (pp (<=)) (H h) (pp (<=)) (A a) fconst c SLF.Pp.args (List.map SLF_LF.from_obj (List.map LF_XLF.from_obj b))
	| DHead (h,a) -> fprintf fmt "@[%a@ :@ %a, @]" (pp (<=)) (H h) SLF.Pp.term ((SLF_LF.from_fam (LF_XLF.from_fam a)))
    ) b ()
    | V(VHead (h,c,l)) -> fprintf fmt "@[%a@ :@ %a@ %a@]" (pp (<=)) (H h) fconst c SLF.Pp.args (List.map SLF_LF.from_obj (List.map LF_XLF.from_obj l))
    | V(VLam (x,a,t)) -> fprintf fmt "@[λ%a@ :@ %a.@ %a@]" variable x SLF.Pp.term ((SLF_LF.from_fam (LF_XLF.from_fam a))) (pp (<=)) (O t)

  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
end

let lift_def x = function
  | Obj(subst, _) ->
      match Varmap.find x subst with
	| DApp (_, _, c, fargs) -> XLF.FConst(c, fargs)
	| DHead (_, a) -> a

let to_def = function
  | Obj(subst, VHead(XLF.HVar x, _, _)) ->
    Varmap.find x subst
  | _ -> assert false			(* TODO erreur *)

let go term p u = match term, p with
  | Obj(_, VLam (x, a, Obj(s, v))), None -> (* TODO que faire de _? *)
    Obj(Varmap.add x u s, v)
  | Obj(_, VHead _), None -> assert false (* TODO erreur *)
  | Obj(s, _), Some (x, n) ->
    match Varmap.find x s with
      | DHead (h, a) -> assert false (* TODO erreur *)
      | DApp (h, l, a, m) ->
	match Varmap.find n l with
	  | VLam (x, a, Obj(s, v)) ->
	    Obj(Varmap.add x u s, v)
	  | VHead _ -> assert false	(* TODO error *)

let bidon, bidon_type =
  let x = Name.gen_variable() in
  let s = Varmap.add x
    (DApp (XLF.HConst(Name.mk_oconst "bidon"), Varmap.empty, Name.mk_fconst "Bidon", []))
    Varmap.empty in
  Obj(s, VHead(XLF.HVar x, Name.mk_fconst "Bidon", [])),
  XLF.FConst(Name.mk_fconst "Bidon", [])
