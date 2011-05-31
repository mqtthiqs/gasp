open Format
open Print
open Name.Pp
open NLF

type entity = 
  | O of obj
  | H of ohead
  | B of subst
  | A of args
  | V of value
  | FA of fatom
  | F of fam

let ent_prec = function
_ -> 10

module S = Name.Varmap

let pp pp : entity printing_fun =
  fun fmt (e:entity) -> match e with
    | O(Obj(s, v)) when S.is_empty s -> pp (<=) fmt (V v)
    | O(Obj(s, v)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) (pp (<=)) (V v)
    | H(XLF.HVar x) -> variable fmt x
    | H(XLF.HConst c) -> oconst fmt c
    | A a -> pr_list pr_spc (fun fmt a -> fprintf fmt "@[%a@]" (pp (<)) (V a)) fmt a
    | B b -> Name.Varmap.fold (fun x d () -> match d with
	| DAtom (h,l,fa) -> fprintf fmt "@[%a@ =@ %a@ %a@ :@ %a, @]@," variable x (pp (<=)) (H h) (pp (<=)) (A l) (pp (<=)) (FA fa)
	| DHead (h,a) -> fprintf fmt "@[%a@ :@ %a, @]" (pp (<=)) (H h) (pp (<=)) (F a)
    ) b ()
    | V(VHead (h,fa)) -> fprintf fmt "@[%a@ :@ %a@]" (pp (<=)) (H h) (pp (<=)) (FA fa)
    | V(VLam (x,a,t)) -> fprintf fmt "@[λ%a@ :@ %a.@ %a@]" variable x (pp (<=)) (F a) (pp (<=)) (O t)
    | F(FProd(x,a,b)) -> fprintf fmt "@[Π%a@ :@ %a.@ %a@]" variable x (pp (<=)) (F a) (pp (<=)) (F b)
    | F(FAtom fa) -> fprintf fmt "@[%a@]" (pp (<=)) (FA fa)
    | FA(s,c,l) -> fprintf fmt "@[%a@ ⊢@ %a@ %a@]" (pp (<=)) (B s) fconst c (pp (<=)) (A l) (* TODO *)

let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
let fam fmt a = assert false
let kind fmt k = assert false
