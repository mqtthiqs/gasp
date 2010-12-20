open Name

module P = Position

module List = struct
  let assoc s l =
    try List.assoc s l with Not_found -> failwith (s^" not found")
end

let bind x a env = match x with
  | Named x -> (x,a) :: env
  | Anonymous -> env

let rec oapp sign env args t : XLF.obj =
  match P.value t with
    | LF.OApp(t,u) ->
	let (u,_) = obj sign env u in
	begin match oapp sign env (u::args) t with
	  | XLF.OConst (c, args, XLF.FProd(x,a,b)) -> XLF.OConst (c, args, b)
	  | XLF.OVar (x, args, XLF.FProd(y,a,b)) -> XLF.OVar (x, args, b)
	  | XLF.OApp (t, args, XLF.FProd(x,a,b)) -> XLF.OApp (t, args, b)
	  | _ -> Errors.over_application (P.position t)
	end
    | LF.OConst c -> 
	begin match List.assoc c sign with
	  | XLF.ODecl a -> XLF.OConst (c, args, a)
	  | _ -> assert false 		(* c est une famille *)
	end
    | LF.OVar x -> XLF.OVar (x, args, List.assoc x env)
    | LF.OLam _ ->
	let (t,a) = obj sign env t in
	XLF.OApp (t, args, a)

and obj sign env t : XLF.obj * XLF.fam =
  match P.value t with
    | LF.OLam (x,a,t) -> 
	let a = fam sign env a in
	let (t,at) = obj sign (bind x a env) t in
	XLF.OLam(x, a, t), XLF.FProd(x, a, at)
    | _ -> 
	match oapp sign env [] t with
	  | XLF.OConst(c,args,a) as t -> t, a
	  | XLF.OVar(c,args,a) as t -> t, a
	  | XLF.OApp(c,args,a) as t -> t, a
	  | _ -> assert false

and fapp sign env args a : XLF.fam =
  match P.value a with
    | LF.FApp (a,t) ->
	let (t,_) = obj sign env t in
	begin match fapp sign env (t::args) a with
	  | XLF.FConst (c, args, XLF.KProd(x,a,k)) -> XLF.FConst (c, args, k)
	  | XLF.FConst (c,_,XLF.KType) -> Errors.over_application (P.position a)
	  | _ -> assert false			       (* ??? *)
	end
    | LF.FConst c -> 
	begin match List.assoc c sign with
	  | XLF.FDecl k -> XLF.FConst (c, args, k)
	  | _ -> assert false 		(* c est un objet *)
	end
    | LF.FProd _ -> assert false	(* prod appliqué *)

and fam sign env a : XLF.fam =
  match P.value a with
    | LF.FProd (x,a,b) ->
	let a = fam sign env a in
	let b = fam sign (bind x a env) b in
	XLF.FProd(x,a,b)
    | LF.FApp _ | LF.FConst _ ->
	match fapp sign env [] a with
	  | XLF.FConst (c, args, k) as a -> a
	  | _ -> assert false		(* impossible *)

and kind sign env k : XLF.kind =
  match P.value k with
    | LF.KType -> XLF.KType
    | LF.KProd(x,a,k) -> 
	let a = fam sign env a in
	let k = kind sign (bind x a env) k in
	XLF.KProd(x, a, k)

let rec sign s = function
  | [] -> s
  | (c, LF.ODecl a) :: tl ->
      sign ((c, XLF.ODecl (fam s [] a)) :: s) tl
  | (c, LF.FDecl k) :: tl -> 
      sign ((c, XLF.FDecl (kind s [] k)) :: s) tl
