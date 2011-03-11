open Name
open NLF

module P = Position

(* Typing: from SLF to LF *)

let term : NLFSign.t -> SLF.term -> LF.entity =
fun sign t ->
  let rec term env {P.value=t; P.position=pos} =
      match t with
	| SLF.Type -> LF.Kind LF.KType
	| SLF.Prod(x,t,u) ->
	    begin match term env t with
	      | LF.Fam a -> 
		  begin match term ((x,a) :: env) u with
		    | LF.Kind k -> LF.Kind(LF.KProd(Named (mk_variable x),a,k))
		    | LF.Fam b -> LF.Fam(LF.FProd(Named (mk_variable x),a,b))
		    | _ -> Errors.not_a_kind_or_fam u
		  end
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.Arr(t,u) ->
	    begin match term env t, term env u with
	      | LF.Fam a, LF.Kind k -> LF.Kind(LF.KProd(Anonymous,a,k))
	      | LF.Fam a, LF.Fam b -> LF.Fam(LF.FProd(Anonymous,a,b))
	      | LF.Fam _, _ -> Errors.not_a_kind_or_fam u
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.Lam(x,t,u) ->
	    begin match term env t with
	      | LF.Fam a ->
		  begin match term ((x,a) :: env) u with
		    | LF.Obj o -> LF.Obj(LF.OLam(Named (mk_variable x),a,o))
		    | _ -> Errors.not_an_obj u
		  end
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.App(t,u) ->
	    begin match term env t, term env u with
	      | LF.Fam a, LF.Obj o -> LF.Fam(LF.FApp(a,o))
	      | LF.Obj o, LF.Obj v -> LF.Obj(LF.OApp(o,v))
	      | _, LF.Obj _ -> Errors.not_a_fam_or_obj t
	      | _ -> Errors.not_an_obj u
	    end
	| SLF.Meta x -> LF.Obj(LF.OMeta (mk_definition x))
	| SLF.Box _ -> assert false	(* TODO *)
	| SLF.Var x ->
	    if List.mem_assoc x env then LF.Obj(LF.OVar (mk_variable x))
	    else 
	      try match NLFSign.find (mk_constant x) sign with
		| NLF.FDecl _ -> LF.Fam (LF.FConst (mk_constant x))
		| NLF.ODecl _ -> LF.Obj (LF.OConst (mk_constant x))
	      with Not_found -> Errors.not_bound pos x
  in
  term [] t

let entry c kont nlfs =
  function SLF.Decl t -> 
    match term nlfs t with
      | LF.Kind k -> 
	  let e = kont nlfs (LF.FDecl k) in
	  Util.if_debug (fun () -> Format.printf "%s :: @[%a@]@." c Pp.entry e);
	  e
      | LF.Fam a -> 
	  let e = kont nlfs (LF.ODecl a) in
	  Util.if_debug (fun () -> Format.printf "%s :: @[%a@]@." c Pp.entry e);
	  e
      | LF.Obj _ -> assert false	(* OK *)

let rec sign kont nlfs s =
    List.fold_left
      (fun nlfs (c,t) -> 
	 NLFSign.add (mk_constant c) (entry c kont nlfs t) nlfs
      ) nlfs s

(* Detyping: from LF to SLF *)

let rec from_obj' = function
  | LF.OConst c -> SLF.Var (of_constant c)
  | LF.OVar x -> SLF.Var (of_variable x)
  | LF.OLam (Anonymous, a, t) -> 
      SLF.Lam ("_", from_fam a, from_obj t)
  | LF.OLam (Named x, a, t) -> SLF.Lam (of_variable x, from_fam a, from_obj t)
  | LF.OApp (t,u) -> SLF.App (from_obj t, from_obj u)
  | LF.OMeta x -> SLF.Meta (of_definition x)
  | LF.OBox (t,p,s) -> SLF.Box (from_obj t, of_variable p, List.map (fun (x,t) -> of_variable x, from_obj t)) s

and from_obj t = P.with_pos P.dummy (from_obj' t)

and from_fam' = function
  | LF.FConst c -> SLF.Var (of_constant c)
  | LF.FProd (Anonymous, a, b) -> SLF.Arr(from_fam a, from_fam b)
  | LF.FProd (Named x, a, b) -> SLF.Prod(of_variable x, from_fam a, from_fam b)
  | LF.FApp (a,t) -> SLF.App (from_fam a, from_obj t)

and from_fam a = P.with_pos P.dummy (from_fam' a)

let rec from_kind' = function
  | LF.KType -> SLF.Type
  | LF.KProd(Anonymous,a,k) -> SLF.Arr(from_fam a, from_kind k)
  | LF.KProd(Named x,a,k) -> SLF.Prod(of_variable x, from_fam a, from_kind k)

and from_kind (k:LF.kind) : SLF.term = 
  P.with_pos P.dummy (from_kind' k)

let from_sign s =
  List.map
    (function
       | (id, LF.FDecl k) -> (of_constant id, SLF.Decl(from_kind k))
       | (id, LF.ODecl a) -> (of_constant id, SLF.Decl(from_fam a))
    ) s
