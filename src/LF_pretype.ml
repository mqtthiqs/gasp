open LF

module P = Position

type taxonomy =
  | Kind of kind
  | Fam of fam
  | Obj of obj

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

(* Typing: from AST to stratified LF *)

let tax_of_term (sign:signature) =
  let rec tax_of_term (env:environ) : LF_AST.term -> taxonomy = 
    fun {P.value=t; P.position=pos} ->
      let p v = P.with_pos pos v in
      match t with
	| LF_AST.Type -> Kind (p KType)
	| LF_AST.Prod(x,t,u) ->
	    begin match tax_of_term env t with
	      | Fam a -> 
		  let env = (P.value x,a) :: env in
		  begin match tax_of_term env u with
		    | Kind k -> Kind(p(KProd(Name x,a,k)))
		    | Fam b -> Fam(p(FProd(Name x,a,b)))
		    | _ -> assert false
		  end
	      | _ -> assert false
	    end
	| LF_AST.Arr(t,u) ->
	    begin match tax_of_term env t, tax_of_term env u with
	      | Fam a, Kind k -> Kind(p(KProd(Anonymous,a,k)))
	      | Fam a, Fam b -> Fam(p(FProd(Anonymous,a,b)))
	      | _ -> assert false
	    end
	| LF_AST.Lam(x,t,u) ->
	    begin match tax_of_term env t with
	      | Fam a ->
		  let env = (P.value x, a) :: env in
		  begin match tax_of_term env u with
		    | Fam b -> Fam(p(FLam(Name x,a,b)))
		    | Obj o -> Obj(p(OLam(Name x,a,o)))
		    | _ -> assert false
		  end
	      | _ -> assert false
	    end
	| LF_AST.App(t,u) ->
	    begin match tax_of_term env t, tax_of_term env u with
	      | Fam a, Obj o -> Fam(p(FApp(a,o)))
	      | Obj o, Obj v -> Obj(p(OApp(o,v)))
	      | _ -> assert false
	    end
	| LF_AST.Var x ->
	    if List.mem_assoc x env then Obj(p (OVar x))
	    else 
	      try match List.assoc x sign with
		| EKind _ -> Fam (p (FConst x))
		| EFam _ -> Obj (p (OConst x))
	      with Not_found -> error_not_bound pos x
  in
  tax_of_term

let sign_of_ast : LF_AST.signature -> signature =
  let rec sign_of_ast (sign:signature) = function
    | [] -> sign
    | (id, t)::tl -> 
	match tax_of_term sign [] t with
	| Fam a -> sign_of_ast ((id, EFam a) :: sign) tl
	| Kind k -> sign_of_ast ((id, EKind k) :: sign) tl
	| _ -> assert false
  in
  sign_of_ast []
