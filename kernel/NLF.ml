open Name

include types of mli with

module NLFEnv = struct
  type key = variable  
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
  type key = definition
  type value = NLF.ohead * NLFArgs.t * constant * NLFArgs.t
  type t = value Defmap.t
  let add x e m = Defmap.add x e m
  let find x m = Defmap.find x m
  let fold f m acc = Defmap.fold f m acc
  let is_empty t = Defmap.is_empty t
  let empty = Defmap.empty
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

and module NLFArgs = struct  
  type key = variable
  type value = NLF.obj
  type t = value Varmap.t * variable list
  let add x e (m,a:t) = Varmap.add x e m, x::a
  let find x (m,a:t) = Varmap.find x m
  let fold f (m,a:t) acc = List.fold_right
    (fun x acc -> f x (Varmap.find x m) acc
    ) a acc
  let is_empty (_, t) = t = []
  let empty = Varmap.empty, []
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
    | H of ohead
    | E of env
    | B of subst
    | A of args
    | S of NLFSign.t

  let ent_prec = function
      _ -> 10

  let pp pp fmt t = 
    let pr_envs e s pr_head fmt () = 
      match NLFEnv.is_empty e, NLFSubst.is_empty s with
	| true, true -> pr_head fmt ()
	| true, false -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) pr_head ()
	| false, true -> fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (E e) pr_head ()
	| false, false -> fprintf fmt "@[%a@ ,@ %a@ ⊢@ %a@]" (pp (<=)) (E e) (pp (<=)) (B s) pr_head ()
    in
    let pr_fhead c fargs fmt () = 
      if NLFArgs.is_empty fargs then constant fmt c else
	fprintf fmt "@[%a@ %a@]" Name.Pp.constant c (pp (<=)) (A fargs) in
    match t with
      | K(KType e) when NLFEnv.is_empty e -> fprintf fmt "@[type@]"
      | K(KType e) -> fprintf fmt "@[%a@ type@]" (pp (<=)) (E e)
      | F(Fam(e,s,c,fargs)) -> fprintf fmt "%a" (pr_envs e s (pr_fhead c fargs)) ()
      | O(Obj(e, s, h, args, c, fargs)) ->
	  let pr_ohead pr_fhead fmt () = 
	    if NLFArgs.is_empty args 
	    then fprintf fmt "%a@ :@ %a" (pp (<=)) (H h) pr_fhead ()
	    else fprintf fmt "%a@ %a@ :@ %a" (pp (<=)) (H h) (pp (<=)) (A args) pr_fhead () in
	  pr_envs e s (pr_ohead (pr_fhead c fargs)) fmt ()
      | O(OMeta(e,s,x,c,fargs)) ->
	    let pr_hd fmt () = fprintf fmt "@[%a@ :@ %a@]" definition x (pr_fhead c fargs) () in
	    pr_envs e s pr_hd fmt ()
      | H(XLF.HVar x) -> variable fmt x
      | H(XLF.HConst c) -> constant fmt c
      | E e -> NLFEnv.fold (fun x a () -> fprintf fmt "@[[%a@ :@ %a]@]@," variable x (pp (<=)) (F a)) e ()
      | A a -> NLFArgs.fold (fun x t () -> fprintf fmt "@[{%a@ =@ %a}@]@," variable x (pp (<=)) (O t)) a ()
      | B b -> NLFSubst.fold (fun x (h,a,c,b) () -> fprintf fmt "@[[%a@ =@ %a@ %a@ :@ %a@ %a]@]@," definition x (pp (<=)) (H h) (pp (<=)) (A a) constant c (pp (<=)) (A b)) b ()
      | S s -> NLFSign.fold
	    (fun c e () -> 
	       match e with
		 | NLF.ODecl a -> fprintf fmt "@[[%a@ :@ %a]@]@," constant c (pp (<=)) (F a)
		 | NLF.FDecl k -> fprintf fmt "@[[%a@ :@ %a]@]@," constant c (pp (<=)) (K k)
	    ) s ()

  let sign fmt s = pr_paren pp ent_prec 100 (<=) fmt (S s)
  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
  let fam fmt s = pr_paren pp ent_prec 100 (<=) fmt (F s)
  let kind fmt s = pr_paren pp ent_prec 100 (<=) fmt (K s)
  let entry fmt = function
    | NLF.FDecl k -> pr_paren pp ent_prec 100 (<=) fmt (K k)
    | NLF.ODecl a -> pr_paren pp ent_prec 100 (<=) fmt (F a)
  let env fmt s = pr_paren pp ent_prec 100 (<=) fmt (E s)

end

let lift_def x = function
  | NLF.OMeta(_, subst, _, _, _)
  | NLF.Obj(_, subst, _, _ , _, _) -> 
      let _, _, c, fargs = NLFSubst.find x subst in
      NLF.Fam(NLFEnv.empty, NLFSubst.empty, c, fargs)

let rec go x = function
  | NLF.OMeta(env,sigma,d,_,_) -> 
      let(h,args,c,fargs) = NLFSubst.find d sigma in
      go x (NLF.Obj(env,sigma,h,args,c,fargs))
  | NLF.Obj(_,_,_,args,_,_) -> NLFArgs.find x args

let bidon = NLF.Obj(NLFEnv.empty,NLFSubst.empty,XLF.HConst(Name.mk_constant"bidon"),NLFArgs.empty,Name.mk_constant"Bidon",NLFArgs.empty)
