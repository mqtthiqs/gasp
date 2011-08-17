open Util
open NLF
open ILF
open Name

module P = Position
module LF = ILF

(* Typing: from SLF to LF *)

let term : NLF.signature -> SLF.term -> ILF.entity =
  fun sign t ->
    let rec term env {P.value=t; P.position=pos} =
	match t with
	  | SLF.Type -> LF.Kind LF.KType
	  | SLF.Prod(x,t,u) ->
	    begin match term env t with
	      | LF.Fam a -> 
		begin match term (Varset.add x env) u with
		  | LF.Kind k -> LF.Kind (LF.KProd (x, a, k))
		  | LF.Fam b -> LF.Fam (LF.FProd (x, a, b))
		  | _ -> Errors.not_a_kind_or_fam u
		end
	      | _ -> Errors.not_a_fam t
	    end
	  | SLF.Lam(x,ty,u) ->
	    begin match term env ty, term (Varset.add x env) u with
	      | LF.Fam a, LF.Obj o -> LF.Obj (LF.OLam (x, a, o))
	      | _ -> Errors.not_an_obj u
	    end

	  | SLF.Def (SLF.Definitions.Open (x, t)) ->
	    begin match term env t with
	      | LF.Obj o -> LF.Obj (LF.ODef (LF.Definitions.Open (x, o)))
	      | LF.Fam a -> LF.Fam (LF.FDef (LF.Definitions.Open (x, a)))
	      | _ -> Errors.not_a_fam_or_obj t
	    end

	  | SLF.Def (SLF.Definitions.Define (defs, t)) ->
	    let env, defs = definitions env defs in
	    begin match term env t with
	      | LF.Obj o -> LF.Obj (LF.ODef (LF.Definitions.Define (defs, o)))
	      | LF.Fam a -> LF.Fam (LF.FDef (LF.Definitions.Define (defs, a)))
	      | _ -> Errors.not_a_fam_or_obj t
	    end

	  | SLF.App(t,u) ->
	    begin match term env t, term env u with
	      | LF.Fam a, LF.Obj o -> LF.Fam(LF.FApp(a,[o]))
	      | LF.Obj o, LF.Obj v -> LF.Obj(LF.OApp(o,[v]))
	      | _, LF.Obj _ -> Errors.not_a_fam_or_obj t
	      | _ -> Errors.not_an_obj u
	    end
	  | SLF.Ident x ->
	    (* if it's in [env] it's a (declared) variable *)
	    if Varset.mem x env
	    then LF.Obj (LF.OVar x)
	    (* if it's in [sign] it's a constant*)
	    else if NLF.mem_fconst (mk_fconst x) sign
	    then LF.Fam (LF.FConst (mk_fconst x))
	    else if NLF.mem_oconst (mk_oconst x) sign
	    then LF.Obj (LF.OConst (mk_oconst x))
	    (* otherwise it might still be a (defined) variable looked up in XLFa *)
	    else LF.Obj (LF.OVar x)

    and definitions env defs =
      List.fold_left (fun (env, defs) (x, ty, t) ->
	let ty = match ty with
	  | None -> None
	  | Some ty ->
	    match term env ty with
	      | LF.Fam a -> Some a
	      | _ -> Errors.not_a_fam ty
	in
	match t with
	  | None -> (Varset.add x env, LF.Definitions.declare x ty defs)
	  | Some t -> 
	    begin match term env t with
	      | LF.Obj o -> 
		(Varset.add x env, LF.Definitions.define x o ty defs)
	      | _ -> Errors.not_an_obj t
	    end)
	(env, LF.Definitions.empty ()) 
	(SLF.Definitions.as_list defs)

    in
    term Varset.empty t

(* Detyping: from LF to SLF *)

let rec from_obj' = function
  | LF.OConst c -> 
    SLF.Ident (of_oconst c)
  | LF.OVar x -> 
    SLF.Ident x
  | LF.OLam (x, ty, t) -> 
    SLF.Lam (x, from_fam ty, from_obj t)
  | LF.OApp (t,us) -> 
    let t = from_obj t in
    let loc x = Position.with_pos (Position.position t) x in
    List.fold_left (fun lhs u -> SLF.App (loc lhs, from_obj u)) (Position.value t) us
  | LF.ODef (LF.Definitions.Open (x, t)) -> 
    SLF.Def (SLF.Definitions.Open (x, from_obj t))
  | LF.ODef (LF.Definitions.Define (defs, t)) -> 
    SLF.Def (SLF.Definitions.Define (from_definitions defs, from_obj t))

and from_definitions defs = 
  List.fold_left (fun defs (x, ty, t) ->
    let ty = match ty with None -> None | Some ty -> Some (from_fam ty) in
    match t with 
      | None -> SLF.Definitions.declare x ty defs 
      | Some t -> SLF.Definitions.define x (from_obj t) ty defs)
    (SLF.Definitions.empty ())
    (LF.Definitions.as_list defs)

and from_obj t = P.with_pos P.dummy (from_obj' t)

and from_fam' = function
  | LF.FConst c -> SLF.Ident (of_fconst c)
  | LF.FProd (x, a, b) -> SLF.Prod (x, from_fam a, from_fam b)
  | LF.FApp (a,t) -> 
    let a = from_fam a in
    let loc x = Position.with_pos (Position.position a) x in
    List.fold_left (fun lhs u -> SLF.App (loc lhs, from_obj u)) (Position.value a) t
  | LF.FDef (LF.Definitions.Open (x, t)) -> 
    SLF.Def (SLF.Definitions.Open (x, from_fam t))
  | LF.FDef (LF.Definitions.Define (defs, t)) -> 
    SLF.Def (SLF.Definitions.Define (from_definitions defs, from_fam t))

and from_fam a = P.with_pos P.dummy (from_fam' a)

let rec from_kind' = function
  | LF.KType -> SLF.Type
  | LF.KProd (x,a,k) -> SLF.Prod (x, from_fam a, from_kind k)

and from_kind (k:LF.kind) : SLF.term = 
  P.with_pos P.dummy (from_kind' k)
