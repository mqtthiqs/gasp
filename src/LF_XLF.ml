open Name

module P = Position

module List = struct
  let assoc s l =
    try List.assoc s l with Not_found -> failwith (s^" not found")
  let map = List.map
end

(* From LF to XLF: sequent-style annotated applications, argument naming *)

let rec oapp sign env t : XLF.obj =
  match P.value t with
    | LF.OApp(t,u) ->
	let (u,_) = obj sign env u in
	begin match oapp sign env t with
	  | XLF.OConst (c, args, XLF.FProd(x,a,b)) -> XLF.OConst (c, (x,u) :: args, b)
	  | XLF.OVar (y, args, XLF.FProd(x,a,b)) -> XLF.OVar (y, (x,u) :: args, b)
	  | XLF.OApp (t, args, XLF.FProd(x,a,b)) -> XLF.OApp (t, (x,u) :: args, b)
	  | XLF.OLam _ -> assert false	(* vrai *)
	  | _ -> Errors.over_application (SLF_LF.from_obj t)
	end
    | LF.OConst c ->
	begin match List.assoc c sign with
	  | XLF.ODecl a -> XLF.OConst (c, [], a)
	  | _ -> Errors.not_an_obj (SLF_LF.from_obj t)
	end
    | LF.OVar x -> XLF.OVar (x, [], List.assoc x env)
    | LF.OLam _ ->
	let (t,a) = obj sign env t in
	XLF.OApp (t, [], a)

and obj sign env t : XLF.obj * XLF.fam =
  match P.value t with
    | LF.OLam (x,a,t) -> 
	let a = fam sign env a in
	let x = variable_for x in
	let (t,at) = obj sign ((x,a) :: env) t in
	XLF.OLam(x, a, t), XLF.FProd(x, a, at)
    | _ -> 
	match oapp sign env t with
	  | XLF.OConst(c,args,a) as t -> t, a
	  | XLF.OVar(c,args,a) as t -> t, a
	  | XLF.OApp(c,args,a) as t -> t, a
	  | _ -> assert false		(* vrai *)

and fapp sign env a : XLF.fam =
  match P.value a with
    | LF.FApp (a,t) ->
	let (t,_) = obj sign env t in
	begin match fapp sign env a with
	  | XLF.FConst (c, args, XLF.KProd(x,a,k)) -> XLF.FConst (c, (x,t) :: args, k)
	  | XLF.FConst (c,_,XLF.KType) -> Errors.over_application (SLF_LF.from_fam a)
	  | _ -> assert false           (* vrai *)
	end
    | LF.FConst c -> 
	begin match List.assoc c sign with
	  | XLF.FDecl k -> XLF.FConst (c, [], k)
	  | _ -> Errors.not_a_fam (SLF_LF.from_fam a) 		(* c est un objet *)
	end
    | LF.FProd _ -> Errors.bad_application (SLF_LF.from_fam a)	(* prod appliqué *)

and fam sign env a : XLF.fam =
  match P.value a with
    | LF.FProd (x,a,b) ->
	let a = fam sign env a in
	let x = variable_for x in
	let b = fam sign ((x,a) :: env) b in
	XLF.FProd(x,a,b)
    | LF.FApp _ | LF.FConst _ ->
	match fapp sign env a with
	  | XLF.FConst (c, args, k) as a -> a
	  | _ -> assert false		(* vrai *)

and kind sign env k : XLF.kind =
  match P.value k with
    | LF.KType -> XLF.KType
    | LF.KProd(x,a,k) -> 
	let a = fam sign env a in
	let x = variable_for x in
	let k = kind sign ((x,a) :: env) k in
	XLF.KProd(x, a, k)

let rec sign (s' : XLF.sign) (s :LF.sign) : XLF.sign = 
  Util.list_map_prefix 
    (fun s -> function
       | c, LF.ODecl a -> c, XLF.ODecl (fam s [] a)
       | c, LF.FDecl k -> c, XLF.FDecl (kind s [] k)
    ) s' s

(* ... and back: *)
exception Done of bool
let rec depends_kind x = function
  | XLF.KType -> false
  | XLF.KProd (y,a,k) when x=y -> false
  | XLF.KProd (y,a,k) -> depends_fam x a || depends_kind x k
and depends_fam x = function
  | XLF.FProd (y,a,b) when x=y -> false
  | XLF.FProd (y,a,b) -> depends_fam x a || depends_fam x b
  | XLF.FConst (c,l,k) -> depends_kind x k || depends_args x l
and depends_args x l = 
  let rec aux = function
    | [] -> false
    | (y,t) :: args when x=y -> raise (Done (aux args || depends_obj x t))
    | (y,t) :: args -> aux args || depends_obj x t
  in try aux l with Done b -> b
and depends_obj x = function
  | XLF.OLam (y,a,t) when x=y -> depends_fam x a
  | XLF.OLam (y,a,t) -> depends_fam x a || depends_obj x t
  | XLF.OVar (y,l,a) when x=y -> true
  | XLF.OVar (y,l,a) -> depends_args x l || depends_fam x a (* ? *)
  | XLF.OConst (c,l,a) -> depends_args x l || depends_fam x a (* ? *)
  | XLF.OApp (t,l,a) -> depends_obj x t || depends_args x l || depends_fam x a

let name_for_obj x t = if depends_obj x t then Named x else Anonymous
let name_for_fam x t = if depends_fam x t then Named x else Anonymous
let name_for_kind x t = if depends_kind x t then Named x else Anonymous

let rec from_fapp' f = function
  | [] -> f
  | (_,t) :: args -> LF.FApp(from_fapp f args, from_obj t)

and from_fapp f args = P.with_pos P.dummy (from_fapp' f args)

and from_oapp' f = function
  | [] -> f
  | (_,t) :: args -> LF.OApp(from_oapp f args, from_obj t)

and from_oapp f args = P.with_pos P.dummy (from_oapp' f args)

and from_fam' = function
  | XLF.FProd(x,a,b) -> LF.FProd(name_for_fam x b, from_fam a, from_fam b)
  | XLF.FConst(c,l,_) -> from_fapp' (LF.FConst c) l

and from_fam a : LF.fam = P.with_pos P.dummy (from_fam' a)

and from_obj' = function
  | XLF.OLam (x,a,t) -> LF.OLam (name_for_obj x t, from_fam a, from_obj t)
  | XLF.OVar (x,l,_) -> from_oapp' (LF.OVar x) l
  | XLF.OConst (c,l,_) -> from_oapp' (LF.OConst c) l
  | XLF.OApp (t,l,_) -> from_oapp' (from_obj' t) l

and from_obj t = P.with_pos P.dummy (from_obj' t)

let rec from_kind' = function
  | XLF.KType -> LF.KType
  | XLF.KProd(x,a,k) -> LF.KProd(name_for_kind x k, from_fam a, from_kind k)

and from_kind t = P.with_pos P.dummy (from_kind' t)

let rec from_sign = 
  List.map 
    (function
       | x, XLF.FDecl k -> x, LF.FDecl (from_kind k)
       | x, XLF.ODecl a -> x, LF.ODecl (from_fam a)
    )
