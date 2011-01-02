open Format
open Pp
open NLF
open NLF

type entity = 
  | K of kind
  | F of fam
  | O of obj
  | FH of fhead
  | OH of ohead
  | E of env
  | S of sign

let ent_prec = function
    _ -> 10

let ident fmt x = fprintf fmt "@[%s@]" x

let pp pp fmt = function
  | K(Kind e) when NLFEnv.is_empty e -> fprintf fmt "kind"
  | K(Kind e) -> fprintf fmt "%a@ kind" (pp (<=)) (E e)
  | F(Fam(e1,FConst(a,e2))) when 
      NLFEnv.is_empty e1 && NLFEnv.is_empty e2 -> 
      fprintf fmt "%a" ident a
  | F(Fam(e,ht)) when NLFEnv.is_empty e -> fprintf fmt "@[%a@ type@]"
      (pp (<=)) (FH ht)
  | F(Fam(e,ht)) -> fprintf fmt "@[%a@ ⊢@ %a@ type@]"
      (pp (<=)) (E e) (pp (<=)) (FH ht)
  | O(Obj(e, ht, ha)) when NLFEnv.is_empty e -> fprintf fmt "@[%a@ :@ %a@]"
      (pp (<=)) (OH ht) (pp (<=)) (FH ha)
  | O(Obj(e, ht, ha)) -> fprintf fmt "@[%a@ ⊢@ %a@ :@ %a@]"
      (pp (<=)) (E e) (pp (<=)) (OH ht) (pp (<=)) (FH ha)
  | FH(FConst(c,a)) when NLFEnv.is_empty a -> fprintf fmt "%a" ident c
  | FH(FConst(c,a)) -> fprintf fmt "%a@ %a"
      ident c (pp (<=)) (E a)
  | OH(OVar(x,a)) when NLFEnv.is_empty a -> fprintf fmt "%a" ident x
  | OH(OVar(x,a)) -> fprintf fmt "%a@ %a"
      ident x (pp (<=)) (E a)
  | OH(OConst(c,a)) when NLFEnv.is_empty a -> fprintf fmt "%a" ident c
  | OH(OConst(c,a)) -> fprintf fmt "%a@ %a"
      ident c (pp (<=)) (E a)
  | OH(OApp(t,a)) when NLFEnv.is_empty a -> fprintf fmt "%a" (pp (<)) (O t)
  | OH(OApp(t,a)) -> fprintf fmt "%a@ %a"
      (pp (<)) (O t) (pp (<=)) (E a)
  | E e ->
      NLFEnv.fold			(* TODO les dépendances! *)
	(fun x e () -> 
	   match e with
	     | NLFEnv.ODecl a -> fprintf fmt "@[[%a@ :@ %a]@]@,"
		 ident x (pp (<=)) (F a)
	     | NLFEnv.ODef t -> fprintf fmt "@[[%a@ =@ %a]@]@,"
		 ident x (pp (<=)) (O t)
	) e ()
  | S s -> 
      NLFSign.fold
	(fun x e () -> 
	   match e with
	     | NLFSign.ODecl a -> fprintf fmt "@[[%a@ :@ %a]@]@,"
		 ident x (pp (<=)) (F a)
	     | NLFSign.FDecl k -> fprintf fmt "@[[%a@ :@ %a]@]@,"
		 ident x (pp (<=)) (K k)
	) s ()

let sign fmt s = pr pp ent_prec 100 (<=) fmt (S s)
let obj fmt s = pr pp ent_prec 100 (<=) fmt (O s)
let fam fmt s = pr pp ent_prec 100 (<=) fmt (F s)
let kind fmt s = pr pp ent_prec 100 (<=) fmt (K s)
let env fmt s = pr pp ent_prec 100 (<=) fmt (E s)
