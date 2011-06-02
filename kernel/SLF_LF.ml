open Util
open Name

module P = Position

(* Typing: from SLF to LF *)

let term : NLF_Sign.t -> SLF.term -> LF.entity =
fun sign t ->
  let rec term env {P.value=t; P.position=pos} =
      match t with
	| SLF.Type -> LF.Kind LF.KType
	| SLF.Prod(x,t,u) ->
	    begin match term env t with
	      | LF.Fam a -> 
		  begin match term (Stringset.add x env) u with
		    | LF.Kind k -> LF.Kind(LF.KProd(Some (mk_variable x),a,k))
		    | LF.Fam b -> LF.Fam(LF.FProd(Some (mk_variable x),a,b))
		    | _ -> Errors.not_a_kind_or_fam u
		  end
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.Arr(t,u) ->
	    begin match term env t, term env u with
	      | LF.Fam a, LF.Kind k -> LF.Kind(LF.KProd(None,a,k))
	      | LF.Fam a, LF.Fam b -> LF.Fam(LF.FProd(None,a,b))
	      | LF.Fam _, _ -> Errors.not_a_kind_or_fam u
	      | _ -> Errors.not_a_fam t
	    end
	| SLF.Lam(Some x,u) ->
	  begin match term (Stringset.add x env) u with
	    | LF.Obj o -> LF.Obj(LF.OLam(Some (mk_variable x),o))
	    | _ -> Errors.not_an_obj u
	  end
	| SLF.Lam(None, u) ->
	  begin match term env u with
	    | LF.Obj o -> LF.Obj(LF.OLam(None, o))
	    | _ -> Errors.not_an_obj u
	  end
	| SLF.App(t,u) ->
	    begin match term env t, term env u with
	      | LF.Fam a, LF.Obj o -> LF.Fam(LF.FApp(a,o))
	      | LF.Obj o, LF.Obj v -> LF.Obj(LF.OApp(o,v))
	      | _, LF.Obj _ -> Errors.not_a_fam_or_obj t
	      | _ -> Errors.not_an_obj u
	    end
	| SLF.Box (t,p,u) ->
	  begin match term env t with
	    | LF.Obj t ->
	      let s = match term env u with
		| LF.Obj u -> u
		| _ -> Errors.not_an_obj u in
	      LF.Obj(LF.OBox(t, Option.map (fun (x,n) -> mk_variable x, n) p, s))
	    | _ -> Errors.not_an_obj t
	  end
	| SLF.Ident x ->
	    (* if it's in [env] it's a (declared) variable *)
	    if Stringset.mem x env
	    then LF.Obj(LF.OVar (mk_variable x))
	    (* if it's in [sign] it's a constant*)
	    else if Fconstmap.mem (mk_fconst x) (fst sign)
	    then LF.Fam (LF.FConst (mk_fconst x))
	    else if Oconstmap.mem (mk_oconst x) (snd sign)
	    then LF.Obj (LF.OConst (mk_oconst x))
	    (* otherwise it might still be a (defined) variable looked up in XLFa *)
	    else LF.Obj(LF.OVar (mk_variable x))
  in
  term Stringset.empty t

(* Detyping: from LF to SLF *)

let rec from_obj' = function
  | LF.OConst c -> SLF.Ident (of_oconst c)
  | LF.OVar x -> SLF.Ident (of_variable x)
  | LF.OLam (None, t) -> SLF.Lam (None, from_obj t)
  | LF.OLam (Some x, t) -> SLF.Lam (Some (of_variable x), from_obj t)
  | LF.OApp (t,u) -> SLF.App (from_obj t, from_obj u)
  | LF.OBox (t,p,u) -> SLF.Box (from_obj t, Option.map (fun (x,n) -> of_variable x, n) p, from_obj u)

and from_obj t = P.with_pos P.dummy (from_obj' t)

and from_fam' = function
  | LF.FConst c -> SLF.Ident (of_fconst c)
  | LF.FProd (None, a, b) -> SLF.Arr(from_fam a, from_fam b)
  | LF.FProd (Some x, a, b) -> SLF.Prod(of_variable x, from_fam a, from_fam b)
  | LF.FApp (a,t) -> SLF.App (from_fam a, from_obj t)

and from_fam a = P.with_pos P.dummy (from_fam' a)

let rec from_kind' = function
  | LF.KType -> SLF.Type
  | LF.KProd(None,a,k) -> SLF.Arr(from_fam a, from_kind k)
  | LF.KProd(Some x,a,k) -> SLF.Prod(of_variable x, from_fam a, from_kind k)

and from_kind (k:LF.kind) : SLF.term = 
  P.with_pos P.dummy (from_kind' k)
