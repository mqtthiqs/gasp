
module P = Position

let rec args_to_obj sign env args t =
  match P.value t with
    | LF.OApp (t,u) -> 
	args_to_obj sign env (obj_to_obj sign env u :: args) t
    | LF.OConst c -> XLF.OConst(c, args)
    | LF.OVar x -> XLF.OVar(x, args)
    | LF.OLam _ -> XLF.OApp(obj_to_obj sign env t, args)
	
and obj_to_obj sign (env:LF.env) t =
  match P.value t with
    | LF.OLam (LF.Name x, a, t) -> 
	XLF.OLam(XLF.Name (P.value x), fam_to_fam sign env a, obj_to_obj sign ((P.value x,a) :: env) t)
    | LF.OLam (LF.Anonymous, a, t) -> 
	XLF.OLam(XLF.Anonymous, fam_to_fam sign env a, obj_to_obj sign env t)
    | _ -> args_to_obj sign env [] t

and args_to_fam sign env args a =
  match P.value a with
    | LF.FApp (a,t) ->
	args_to_fam sign env (obj_to_obj sign [] t :: args) a
    | LF.FConst c -> 
	XLF.FConst(c, args)
    | LF.FProd _ -> assert false	(* prod appliqué *)

and fam_to_fam sign env a =
  match P.value a with
    | LF.FProd (LF.Name x,a,b) -> 
	XLF.FProd(XLF.Name (P.value x), fam_to_fam sign env a, fam_to_fam sign ((P.value x,a) :: env) b)
    | LF.FProd (LF.Anonymous,a,b) -> 
	XLF.FProd(XLF.Anonymous, fam_to_fam sign env a, fam_to_fam sign env b)
    | _ -> args_to_fam sign env [] a

let rec kind_to_kind sign env k =
  match P.value k with
    | LF.KType -> XLF.KType
    | LF.KProd (LF.Name x,a,k) -> 
	XLF.KProd (XLF.Name (P.value x), fam_to_fam sign env a, kind_to_kind sign ((P.value x, a) :: env) k)
    | LF.KProd (LF.Anonymous,a,k) -> 
	XLF.KProd (XLF.Anonymous, fam_to_fam sign env a, kind_to_kind sign env k)
