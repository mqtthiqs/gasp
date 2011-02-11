open Format
open Pp
open NLF
open NLF

type entity = 
  | K of kind
  | F of fam
  | O of obj
  | H of head
  | E of env
  | S of sign

let ent_prec = function
    _ -> 10

let ident fmt x = fprintf fmt "@[%s@]" x

let pp pp fmt = function
  | K(KType e) when NLFEnv.is_empty e -> fprintf fmt "@[type@]"
  | K(KType e) -> fprintf fmt "@[%a@ type@]" (pp (<=)) (E e)
  | F(Fam(e1,a,e2)) -> 
      if NLFEnv.is_empty e1 then
	if NLFEnv.is_empty e2 then ident fmt a
	else fprintf fmt "@[%a@ %a@]" ident a (pp (<=)) (E e2)
      else fprintf fmt "@[%a@ ⊢@ %a@ %a@]"
	(pp (<=)) (E e1) ident a (pp (<=)) (E e2)
  | O(Obj(e, h, args, a, fargs)) -> 
	if NLFEnv.is_empty e then
	  fprintf fmt "@[%a@ %a@ :@ %a@ %a@]" 
	    (pp (<=)) (H h) (pp (<=)) (E args) ident a (pp (<=)) (E fargs)
  | H(HVar x) -> ident fmt x
  | H(HConst c) -> ident fmt c
  | H(HObj t) -> pp (<) fmt (O t)
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
