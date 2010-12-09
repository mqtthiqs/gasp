type constant = string
type variable = string

type name' =
  | Name of variable
  | Anonymous

type name = name' Position.located

type fam' =
  | FConst of constant
  | FProd of name * fam * fam
  | FLam of name * fam * fam
  | FApp of fam * obj

and fam = fam' Position.located

and obj' = 
  | OConst of constant
  | OVar of variable
  | OLam of name * fam * obj
  | OApp of obj * obj

and obj = obj' Position.located

type kind' =
  | KType
  | KProd of name * fam * kind

and kind = kind' Position.located

type entry =
  | EKind of kind
  | EFam of fam

type signature = (constant * entry) list
type environ = (variable * fam) list

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

let name_of_ident =
  fun {P.value=id; P.position=pos} ->
    P.with_pos pos (Name id)

let tax_of_term sign =
  let rec tax_of_term (env:environ) : LF_AST.term -> taxonomy = 
    fun {P.value=t; P.position=pos} ->
      let p v = P.with_pos pos v in
      match t with
	| LF_AST.Type -> Kind (P.with_pos pos KType)
	| LF_AST.Prod(x,t,u) ->
	    begin match tax_of_term env t with
	      | Fam a -> 
		  begin match tax_of_term ((P.value x,a)::env) u with
		    | Kind k -> Kind(p(KProd(name_of_ident x,a,k)))
		    | Fam b -> Fam(p(FProd(name_of_ident x,a,b)))
		    | _ -> assert false
		  end
	      | _ -> assert false
	    end
	| LF_AST.Arr(t,u) ->
	    begin match tax_of_term env t, tax_of_term env u with
	      | Fam a, Kind k -> Kind(p(KProd(p Anonymous,a,k)))
	      | Fam a, Fam b -> Fam(p(FProd(p Anonymous,a,b)))
	      | _ -> error_not_bound pos "coucou"
	    end
	| LF_AST.Lam(x,t,u) ->
	    begin match tax_of_term env t with
	      | Fam a ->
		  begin match tax_of_term ((P.value x, a) :: env) u with
		    | Fam b -> Fam(p(FLam(name_of_ident x,a,b)))
		    | Obj o -> Obj(p(OLam(name_of_ident x,a,o)))
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
	    let (x,p) = P.destruct x in
	    if List.mem_assoc x env then Obj(P.with_pos p (OVar x))
	    else 
	      try match List.assoc x sign with
		| EKind _ -> Fam (P.with_pos p (FConst x))
		| EFam _ -> Obj (P.with_pos p (OConst x))
	      with Not_found -> error_not_bound p x
  in
  tax_of_term

let sign_of_ast : LF_AST.signature -> signature =
  let rec sign_of_ast (sign:signature) = function
    | [] -> []
    | (id, t)::tl -> match tax_of_term sign [] t with
	| Fam a -> sign_of_ast ((P.value id, EFam a)::sign) tl
	| Kind k -> sign_of_ast ((P.value id, EKind k)::sign) tl
	| _ -> assert false
  in
  sign_of_ast []
