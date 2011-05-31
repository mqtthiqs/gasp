include module of mli using

module Pp = struct
  open Format
  open Print
  open Name.Pp

  type entity = 
    | K of kind
    | F of fam
    | O of obj
    | H of ohead
    | B of subst
    | A of args
    | V of value

  let ent_prec = function
      _ -> 10

 let pp pp : entity printing_fun =
    let pr_fhead fmt (c, l) : unit =
      if l = [] then fconst fmt c else
    	fprintf fmt "@[%a@ %a@]" fconst c (pp (<=)) (A l) in
    fun fmt (e:entity) -> match e with
    | K(KType) -> fprintf fmt "@[type@]"
    | K(KProd(x,a,k)) ->
      fprintf fmt "@[Π%a@ :@ %a. %a@]" variable x (pp (<=)) (F a) (pp (<=)) (K k)
    | F(FAtom(s,c,l)) when s = [] -> fprintf fmt "@[%a@]" pr_fhead (c, l)
    | F(FAtom(s,c,l)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) pr_fhead (c, l)
    | F(FProd(x,a,b)) ->
      fprintf fmt "@[Π%a@ :@ %a. %a@]" variable x (pp (<=)) (F a) (pp (<=)) (F b)
    | O(Obj(s, v)) when s = [] -> pp (<=) fmt (V v)
    | O(Obj(s, v)) -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) (pp (<=)) (V v)
    | O(OBox(t,p,u)) -> assert false
    | H(XLF.HVar x) -> variable fmt x
    | H(XLF.HConst c) -> oconst fmt c
    | A a -> pr_list pr_spc (fun fmt a -> fprintf fmt "@[%a@]" (pp (<=)) (V a)) fmt a
    | B b -> List.fold_left
      begin fun () (x, (h,l)) ->
	fprintf fmt "@[[%a@ =@ %a@ %a]@]@," variable x (pp (<=)) (H h) (pp (<=)) (A l)
      end () b
    | V(VHead h) -> fprintf fmt "@[%a@]" (pp (<=)) (H h)
    | V(VLam (x,t)) -> fprintf fmt "@[λ%a.@ %a@]" variable x (pp (<=)) (O t)

  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
  let fam fmt s = pr_paren pp ent_prec 100 (<=) fmt (F s)
  let kind fmt s = pr_paren pp ent_prec 100 (<=) fmt (K s)
end
