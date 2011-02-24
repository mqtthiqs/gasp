open Name

include types of mli with

module NLFEnv = struct
  
  type entry =
    | ODecl of NLF.fam
    | ODef of NLF.obj

  type t = entry Varmap.t * variable list

  let add (m,a:t) x e = Varmap.add x e m, x::a
  let find (m,a:t) x = 
    (* try Varmap.find x m with Not_found -> failwith ("not found "^x) *)
    Varmap.find x m
  let fold f (m,a:t) acc = List.fold_left
    (fun acc x -> f x (Varmap.find x m) acc
    ) acc a
  let merge e1 e2 =
    fold
      (fun x e acc -> 
	 try 
	   match e, find acc x with
	     | ODecl _, ODef t -> add acc x (ODef t)
	     | ODecl _, ODecl a -> add acc x (ODecl a)
	     | _ -> assert false
	 with Not_found ->
	   add acc x e			(* TODO ajouter les dépendances *)
      ) e2 e1
  let clear(m,_) = m, []
  let is_empty (_, t) = t = []
  let empty = Varmap.empty, []
end

and module NLFSign = struct
  
  type entry =
    | FDecl of NLF.kind
    | ODecl of NLF.fam
	
  type t = entry Constmap.t
      
  let add x e env = Constmap.add x e env
  let find x env = Constmap.find x env
  let fold f env acc = Constmap.fold f env acc
  let empty = Constmap.empty
end

and module Pp = struct
  open Format
  open Print
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

end

let lift = function NLF.Obj(env, h, args , a, fargs) -> NLF.Fam(env, a, fargs)

