open Name

include types of mli with

module ALFSubst = struct
  type key = definition
  type value = ALF.ohead * ALF.args * constant * ALF.args
  type t = value Defmap.t
  let add x e m = Defmap.add x e m
  let find x m = Defmap.find x m
  let fold f m acc = Defmap.fold f m acc
  let is_empty t = Defmap.is_empty t
  let empty = Defmap.empty
end

and module ALFSign = struct
  type key = constant
  type value = ALF.entry
  type t = value Constmap.t
      
  let add x e env = Constmap.add x e env
  let find x env = Constmap.find x env
  let fold f env acc = Constmap.fold f env acc
  let empty = Constmap.empty
  let is_empty = Constmap.is_empty
end

and module ALFArgs = struct  
  type key = variable
  type value = ALF.arg
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
  open ALF

  type entity = 
    | K of kind
    | F of fam
    | O of obj
    | H of ohead
    | B of subst
    | A of args
    | S of ALF.sign

  let ent_prec = function
      _ -> 10

  let pp pp fmt t = 
    let pr_envs s pr_head fmt () =
      if ALFSubst.is_empty s
      then pr_head fmt ()
      else fprintf fmt "@[%a@ ⊢@ %a@]" (pp (<=)) (B s) pr_head ()
    in
    let pr_fhead c fargs fmt () = 
      if fargs = [] then constant fmt c else
	fprintf fmt "@[%a@ %a@]" Name.Pp.constant c (pp (<=)) (A fargs) in
    match t with
      | K(KType) -> fprintf fmt "@[type@]"
      | K(KProd (x,a,k)) -> fprintf fmt "@[Π%a@ :@ %a.@ %a@]" Name.Pp.variable x (pp (<=)) (F a) (pp (<=)) (K k)
      | F(FHead(FConst(s,c,fargs))) -> fprintf fmt "%a" (pr_envs s (pr_fhead c fargs)) ()
      | O(Obj(e, s, h, args, c, fargs)) ->
	  let pr_ohead pr_fhead fmt () = 
	    if ALFArgs.is_empty args 
	    then fprintf fmt "%a@ :@ %a" (pp (<=)) (H h) pr_fhead ()
	    else fprintf fmt "%a@ %a@ :@ %a" (pp (<=)) (H h) (pp (<=)) (A args) pr_fhead () in
	  pr_envs e s (pr_ohead (pr_fhead c fargs)) fmt ()
      | O(OMeta(e,s,x,c,fargs)) ->
	    let pr_hd fmt () = fprintf fmt "@[%a@ :@ %a@]" definition x (pr_fhead c fargs) () in
	    pr_envs e s pr_hd fmt ()
      | H(HVar x) -> variable fmt x
      | H(HConst c) -> constant fmt c
      | E e -> ALFEnv.fold (fun x a () -> fprintf fmt "@[[%a@ :@ %a]@]@," variable x (pp (<=)) (F a)) e ()
      | A a -> ALFArgs.fold (fun x t () -> fprintf fmt "@[{%a@ =@ %a}@]@," variable x (pp (<=)) (O t)) a ()
      | B b -> ALFSubst.fold (fun x (h,a,c,b) () -> fprintf fmt "@[[%a@ =@ %a@ %a@ :@ %a@ %a]@]@," definition x (pp (<=)) (H h) (pp (<=)) (A a) constant c (pp (<=)) (A b)) b ()
      | S s -> ALFSign.fold
	    (fun c e () -> 
	       match e with
		 | ALF.ODecl a -> fprintf fmt "@[[%a@ :@ %a]@]@," constant c (pp (<=)) (F a)
		 | ALF.FDecl k -> fprintf fmt "@[[%a@ :@ %a]@]@," constant c (pp (<=)) (K k)
	    ) s ()

  let sign fmt s = pr_paren pp ent_prec 100 (<=) fmt (S s)
  let obj fmt s = pr_paren pp ent_prec 100 (<=) fmt (O s)
  let fam fmt s = pr_paren pp ent_prec 100 (<=) fmt (F s)
  let kind fmt s = pr_paren pp ent_prec 100 (<=) fmt (K s)
  let entry fmt = function
    | ALF.FDecl k -> pr_paren pp ent_prec 100 (<=) fmt (K k)
    | ALF.ODecl a -> pr_paren pp ent_prec 100 (<=) fmt (F a)
  let env fmt s = pr_paren pp ent_prec 100 (<=) fmt (E s)

end
