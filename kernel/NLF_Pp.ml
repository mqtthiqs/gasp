open Format
open Print
open Name
open Name.Pp
open NLF

type entity = 
  | O of obj
  | H of head
  | B of subst
  | A of args
  | V of value
  | P of fatom
  | F of fam
  | K of kind

let ent_prec = function
_ -> 10

module S = Name.Varmap

let pp pp : entity printing_fun =
  fun fmt (e:entity) -> match e with
    | O(Obj(s, v)) when S.is_empty s -> pp (<=) fmt (V v)
    | O(Obj(s, v)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) (pp (<=)) (V v)
    | H(Var x) -> variable fmt x
    | H(Cst c) -> oconst fmt c
    | A a -> pr_list pr_spc (fun fmt a -> fprintf fmt "@[%a@]" (pp (<)) (V a)) fmt a
    | B b -> S.fold (fun x d () -> match d with
	| DAtom (h,l,fa) -> fprintf fmt "@[%a@ =@ %a@ %a@ :@ %a, @]@," variable x (pp (<=)) (H h) (pp (<=)) (A l) (pp (<=)) (P fa)
	| DHead (h,a) -> fprintf fmt "@[%a@ :@ %a, @]" (pp (<=)) (H h) (pp (<=)) (F a)
    ) b ()
    | V(VHead (h,fa)) -> fprintf fmt "@[%a@ :@ %a@]" (pp (<=)) (H h) (pp (<=)) (P fa)
    | V(VLam (x,a,t)) -> fprintf fmt "@[[%a]@ :@ %a.@ %a@]" variable x (pp (<=)) (F a) (pp (<=)) (O t)
    | F(FProd(x,a,b)) -> fprintf fmt "@[Π%a@ :@ %a.@ %a@]" variable x (pp (<=)) (F a) (pp (<=)) (F b)
    | F(FAtom fa) -> fprintf fmt "@[%a@]" (pp (<=)) (P fa)
    | P(s,c,l) when S.is_empty s -> fprintf fmt "@[%a@ %a@]" fconst c (pp (<=)) (A l)
    | P(s,c,l) -> fprintf fmt "@[%a@ ⊢@ %a@ %a@]" (pp (<=)) (B s) fconst c (pp (<=)) (A l)
    | K(KType) -> fprintf fmt "@[type@]"
    | K(KProd(x,a,k)) -> fprintf fmt "@[Π%a@ :@ %a.@ %a@]" variable x (pp (<=)) (F a) (pp (<=)) (K k)


let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
let fam fmt a = pr_paren pp ent_prec 100 (<=) fmt (F a)
let kind fmt k = pr_paren pp ent_prec 100 (<=) fmt (K k)
let subst fmt k = pr_paren pp ent_prec 100 (<=) fmt (B k)
