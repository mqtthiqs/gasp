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
    | A a -> pr_list pr_spc (fun fmt (_, a) -> fprintf fmt "@[%a@]" (pp (<)) (V a)) fmt a
    | B b -> Varmap.fold (fun x d () -> match d with
	| DAtom (h,a,(_,(c,b))) -> fprintf fmt "@[%a@ =@ %a@ %a@ :@ %a@ %a, @]@," variable x (pp (<=)) (H h) (pp (<=)) (A a) fconst c SLF.Pp.args (List.map SLF_LF.from_obj (List.map LF_XLF.from_obj b))
	| DHead (h,(_,a)) -> fprintf fmt "@[%a@ :@ %a, @]" (pp (<=)) (H h) SLF.Pp.term ((SLF_LF.from_fam (LF_XLF.from_fam a)))
    ) b ()
    | V(VHead (h,(_,(c,l)))) -> fprintf fmt "@[%a@ :@ %a@ %a@]" (pp (<=)) (H h) fconst c SLF.Pp.args (List.map SLF_LF.from_obj (List.map LF_XLF.from_obj l))
    | V(VLam (x,(_,a),t)) -> fprintf fmt "@[λ%a@ :@ %a.@ %a@]" variable x SLF.Pp.term ((SLF_LF.from_fam (LF_XLF.from_fam a))) (pp (<=)) (O t)

  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
end

let lift_def x = function
  | Obj(subst, _) ->
      match Varmap.find x subst with
	| DAtom (_, _, (b, (c, m))) -> b, XLF.FAtom(c, m)
	| DHead (_, a) -> a

let to_def = function
  | Obj(subst, VHead(XLF.HVar x, (_, _))) ->
    Varmap.find x subst
  | _ -> assert false			(* TODO erreur *)

let go term p = match term, p with
  | Obj(_, t), None -> t                (* TODO que faire de _? *)
  | Obj(s, _), Some (x, n) ->
    try match Varmap.find x s with
      | DHead (h, a) -> failwith "position is not an application"
      | DAtom (h, l, (a, m)) -> snd (List.nth l n)
    with Not_found -> failwith ("go: variable not found "^(of_variable x))

let bind x d = function
  | Obj (s, v) -> Obj (Varmap.add x d s, v)
