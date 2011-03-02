open Name

include types of mli with

module NLFEnv = struct  
  type value = NLF.fam
  type t = value Varmap.t * variable list
  let add x e (m,a:t) = Varmap.add x e m, x::a
  let find x (m,a:t) = Varmap.find x m
  let fold f (m,a:t) acc = List.fold_left
    (fun acc x -> f x (Varmap.find x m) acc
    ) acc a
  let is_empty (_, t) = t = []
  let empty = Varmap.empty, []
end

and module NLFSubst = struct  
  type value = NLF.ohead * NLFArgs.t * constant * NLFArgs.t
  type t = value Varmap.t
  let add x e m = Varmap.add x e m
  let find x m = Varmap.find x m
  let fold f m acc = Varmap.fold f m acc
  let is_empty t = Varmap.is_empty t
  let empty = Varmap.empty
end

and module NLFSign = struct
  type value = NLF.entry
  type t = value Constmap.t
      
  let add x e env = Constmap.add x e env
  let find x env = Constmap.find x env
  let fold f env acc = Constmap.fold f env acc
  let empty = Constmap.empty
  let is_empty = Constmap.is_empty
end

and module NLFArgs = struct
  type value = NLF.obj
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
  open NLF

  type entity = 
    | K of kind
    | F of fam
    | O of obj
    | H of ohead
    | E of env
    | B of subst
    | A of args
    | S of NLFSign.t

  let ent_prec = function
      _ -> 10

  let ident fmt x = fprintf fmt "@[%s@]" x

  let pp pp fmt = function
    | K(KType e) when NLFEnv.is_empty e -> fprintf fmt "@[type@]"
    | K(KType e) -> fprintf fmt "@[%a@ type@]" (pp (<=)) (E e)
    | F(Fam(e,s1,a,fargs)) -> 
	let pr_head fmt () = if NLFArgs.is_empty fargs then ident fmt a else
	  fprintf fmt "@[%a@ %a@]" ident a (pp (<=)) (A fargs) in
	begin match NLFEnv.is_empty e, NLFSubst.is_empty s1 with
	  | true, true -> pr_head fmt ()
	  | true, false -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (E e) pr_head ()
	  | false, true -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s1) pr_head ()
	  | false, false -> ()
	end
    | O(Obj(e, s, h, args, a, fargs)) -> 
	if NLFEnv.is_empty e then
	  fprintf fmt "@[%a@ %a@ :@ %a@ %a@]" 
	    (pp (<=)) (H h) (pp (<=)) (A args) ident a (pp (<=)) (A fargs)
    | H(HVar x) -> ident fmt x
    | H(HConst c) -> ident fmt c
    | H(HDef c) -> ident fmt c
    | E e ->
	NLFEnv.fold
	  (fun x a () -> 
	     fprintf fmt "@[[%a@ :@ %a]@]@,"
	       ident x (pp (<=)) (F a)
	  ) e ()
    | A a ->
	NLFArgs.fold
	  (fun x t () -> 
	     fprintf fmt "@[[%a@ :@ %a]@]@,"
	       ident x (pp (<=)) (O t)
	  ) a ()
    | B b -> 
	NLFSubst.fold
	  (fun x (h,a,c,b) () -> 
	     fprintf fmt "@[[%a@ =@ %a@ %a@ :@ %a@ %a]@]@,"
	       ident x (pp (<=)) (H h) (pp (<=)) (A a) ident x (pp (<=)) (A b)
	  ) b ()
    | S s -> 
	NLFSign.fold
	  (fun x e () -> 
	     match e with
	       | NLF.ODecl a -> fprintf fmt "@[[%a@ :@ %a]@]@,"
		   ident x (pp (<=)) (F a)
	       | NLF.FDecl k -> fprintf fmt "@[[%a@ :@ %a]@]@,"
		   ident x (pp (<=)) (K k)
	  ) s ()

  let sign fmt s = pr pp ent_prec 100 (<=) fmt (S s)
  let obj fmt s = pr pp ent_prec 100 (<=) fmt (O s)
  let fam fmt s = pr pp ent_prec 100 (<=) fmt (F s)
  let kind fmt s = pr pp ent_prec 100 (<=) fmt (K s)
  let env fmt s = pr pp ent_prec 100 (<=) fmt (E s)

end

let lift = function NLF.Obj(env, subst, h, args , a, fargs) -> NLF.Fam(env, subst, a, fargs)
