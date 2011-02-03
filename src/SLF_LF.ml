open Name
open NLF

module P = Position

(* Typing: from SLF to LF *)

let term : NLF.sign -> SLF.term -> LF.entity =
fun sign t ->
  let rec term env = 
    fun {P.value=t; P.position=pos} ->
      let p v = P.with_pos pos v in
      match t with
	| SLF.Type -> LF.Kind (p LF.KType)
	| SLF.Prod(x,t,u) ->
	    begin match term env t with
	      | LF.Fam a -> 
		  begin match term ((x,a) :: env) u with
		    | LF.Kind k -> LF.Kind(p(LF.KProd(Named x,a,k)))
		    | LF.Fam b -> LF.Fam(p(LF.FProd(Named x,a,b)))
		    | _ -> Errors.not_a_kind_or_fam u
		  end
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.Arr(t,u) ->
	    begin match term env t, term env u with
	      | LF.Fam a, LF.Kind k -> LF.Kind(p(LF.KProd(Anonymous,a,k)))
	      | LF.Fam a, LF.Fam b -> LF.Fam(p(LF.FProd(Anonymous,a,b)))
	      | LF.Fam _, _ -> Errors.not_a_kind_or_fam u
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.Lam(x,t,u) ->
	    begin match term env t with
	      | LF.Fam a ->
		  begin match term ((x,a) :: env) u with
		    | LF.Obj o -> LF.Obj(p(LF.OLam(Named x,a,o)))
		    | _ -> Errors.not_an_obj u
		  end
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.App(t,u) ->
	    begin match term env t, term env u with
	      | LF.Fam a, LF.Obj o -> LF.Fam(p(LF.FApp(a,o)))
	      | LF.Obj o, LF.Obj v -> LF.Obj(p(LF.OApp(o,v)))
	      | _, LF.Obj _ -> Errors.not_a_fam_or_obj t
	      | _ -> Errors.not_an_obj u
	    end
	| SLF.Meta x -> LF.Obj(p (LF.OMeta x))
	| SLF.Var x ->
	    if List.mem_assoc x env then LF.Obj(p (LF.OVar x))
	    else 
	      try match NLFSign.find sign x with
		| NLFSign.FDecl _ -> LF.Fam (p (LF.FConst x))
		| NLFSign.ODecl _ -> LF.Obj (p (LF.OConst x))
	      with Not_found -> Errors.not_bound pos x
  in
  term [] t

let entry kont nlfs =
  function SLF.Decl t -> 
    match term nlfs t with
      | LF.Kind k -> kont nlfs (LF.FDecl k)
      | LF.Fam a -> kont nlfs (LF.ODecl a)
      | LF.Obj _ -> assert false	(* OK *)

let rec sign kont nlfs s =
    List.fold_left
      (fun nlfs (c,t) -> 
	 NLFSign.add nlfs c (entry kont nlfs t)
      ) nlfs s

(* Detyping: from LF to SLF *)

let rec from_obj' : LF.obj' -> SLF.term' = function
  | LF.OConst c -> SLF.Var c
  | LF.OVar c -> SLF.Var c
  | LF.OLam (Anonymous, a, t) -> 
      SLF.Lam ("_", from_fam a, from_obj t)
  | LF.OLam (Named x, a, t) -> SLF.Lam (x, from_fam a, from_obj t)
  | LF.OApp (t,u) -> SLF.App (from_obj t, from_obj u)
  | LF.OMeta x -> SLF.Meta x

and from_obj t = P.map from_obj' t

and from_fam' : LF.fam' -> SLF.term' = function
  | LF.FConst c -> SLF.Var c
  | LF.FProd (Anonymous, a, b) -> SLF.Arr(from_fam a, from_fam b)
  | LF.FProd (Named x, a, b) -> SLF.Prod(x, from_fam a, from_fam b)
  | LF.FApp (a,t) -> SLF.App (from_fam a, from_obj t)

and from_fam a =
  P.map from_fam' a

let rec from_kind' : LF.kind' -> SLF.term' = function
  | LF.KType -> SLF.Type
  | LF.KProd(Anonymous,a,k) -> SLF.Arr(from_fam a, from_kind k)
  | LF.KProd(Named x,a,k) -> SLF.Prod(x, from_fam a, from_kind k)

and from_kind (k:LF.kind) : SLF.term = 
  P.with_pos (P.position k) (from_kind' (P.value k))

let from_sign s =
  List.map
    (function
       | (id, LF.FDecl k) -> (id, SLF.Decl(from_kind k))
       | (id, LF.ODecl a) -> (id, SLF.Decl(from_fam a))
    ) s
