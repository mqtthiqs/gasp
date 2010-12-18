module P = Position

(* Typing errors *)

let type_error pos msg = 
  let as_string f = 
    let b = Buffer.create 13 in 
    let fmt = Format.formatter_of_buffer b in
    f fmt;
    Format.pp_print_flush fmt ();
    Buffer.contents b in
  Error.error "during type checking" pos (as_string msg)

let error_not_bound pos x =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Variable %s is not bound.@]@." x)

(* Typing: from SLF to LF *)

let term_to_entity sign =
  let rec term_to_entity env = 
    fun {P.value=t; P.position=pos} ->
      let p v = P.with_pos pos v in
      match t with
	| SLF.Type -> LF.Kind (p LF.KType)
	| SLF.Prod(x,t,u) ->
	    begin match term_to_entity env t with
	      | LF.Fam a -> 
		  let env = (P.value x,a) :: env in
		  begin match term_to_entity env u with
		    | LF.Kind k -> LF.Kind(p(LF.KProd(LF.Name x,a,k)))
		    | LF.Fam b -> LF.Fam(p(LF.FProd(LF.Name x,a,b)))
		    | _ -> assert false
		  end
	      | _ -> assert false
	    end
	| SLF.Arr(t,u) ->
	    begin match term_to_entity env t, term_to_entity env u with
	      | LF.Fam a, LF.Kind k -> LF.Kind(p(LF.KProd(LF.Anonymous,a,k)))
	      | LF.Fam a, LF.Fam b -> LF.Fam(p(LF.FProd(LF.Anonymous,a,b)))
	      | _ -> assert false
	    end
	| SLF.Lam(x,t,u) ->
	    begin match term_to_entity env t with
	      | LF.Fam a ->
		  let env = (P.value x, a) :: env in
		  begin match term_to_entity env u with
		    | LF.Fam b -> LF.Fam(p(LF.FLam(LF.Name x,a,b)))
		    | LF.Obj o -> LF.Obj(p(LF.OLam(LF.Name x,a,o)))
		    | _ -> assert false
		  end
	      | _ -> assert false
	    end
	| SLF.App(t,u) ->
	    begin match term_to_entity env t, term_to_entity env u with
	      | LF.Fam a, LF.Obj o -> LF.Fam(p(LF.FApp(a,o)))
	      | LF.Obj o, LF.Obj v -> LF.Obj(p(LF.OApp(o,v)))
	      | _ -> assert false
	    end
	| SLF.Var x ->
	    if List.mem_assoc x env then LF.Obj(p (LF.OVar x))
	    else 
	      try match List.assoc x sign with
		| LF.EKind _ -> LF.Fam (p (LF.FConst x))
		| LF.EFam _ -> LF.Obj (p (LF.OConst x))
	      with Not_found -> error_not_bound pos x
  in
  term_to_entity

let sign_to_sign s =
  let rec sign_of_ast (sign) = function
    | [] -> sign
    | (id, SLF.Decl t)::tl -> 
	match term_to_entity sign [] t with
	| LF.Fam a -> sign_of_ast ((id, LF.EFam a) :: sign) tl
	| LF.Kind k -> sign_of_ast ((id, LF.EKind k) :: sign) tl
	| _ -> assert false
  in
  List.rev (sign_of_ast [] s)

(* Detyping: from LF to SLF *)

let rec term'_from_obj' : LF.obj' -> SLF.term' = function
  | LF.OConst c -> SLF.Var c
  | LF.OVar c -> SLF.Var c
  | LF.OLam (LF.Anonymous, a, t) -> 
      SLF.Lam (P.with_pos P.dummy "_", term_from_fam a, term_from_obj t) (* TODO: "_" *)
  | LF.OLam (LF.Name x, a, t) -> SLF.Lam (x, term_from_fam a, term_from_obj t)
  | LF.OApp (t,u) -> SLF.App (term_from_obj t, term_from_obj u)

and term_from_obj t = P.map term'_from_obj' t

and term'_from_fam' : LF.fam' -> SLF.term' = function
  | LF.FConst c -> SLF.Var c
  | LF.FProd (LF.Anonymous, a, b) -> SLF.Arr(term_from_fam a, term_from_fam b)
  | LF.FProd (LF.Name x, a, b) -> SLF.Prod(x, term_from_fam a, term_from_fam b)
  | LF.FLam (LF.Anonymous, a, b) -> 
      SLF.Lam (P.with_pos P.dummy "_", term_from_fam a, term_from_fam b) (* TODO: "_" *)
  | LF.FLam (LF.Name x, a, b) -> SLF.Lam (x, term_from_fam a, term_from_fam b)
  | LF.FApp (a,t) -> SLF.App (term_from_fam a, term_from_obj t)

and term_from_fam a =
  P.map term'_from_fam' a

let rec term'_from_kind' : LF.kind' -> SLF.term' = function
  | LF.KType -> SLF.Type
  | LF.KProd(LF.Anonymous,a,k) -> SLF.Arr(term_from_fam a, term_from_kind k)
  | LF.KProd(LF.Name x,a,k) -> SLF.Prod(x, term_from_fam a, term_from_kind k)

and term_from_kind (k:LF.kind) : SLF.term = 
  P.with_pos (P.position k) (term'_from_kind' (P.value k))

let sign_from_sign s =
  List.map
    (function
       | (id, LF.EKind k) -> (id, SLF.Decl(term_from_kind k))
       | (id, LF.EFam a) -> (id, SLF.Decl(term_from_fam a))
    ) s
