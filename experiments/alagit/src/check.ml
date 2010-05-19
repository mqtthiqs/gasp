open AST

let ( ! ) = Position.value
let pos_of = Position.position

(* 
 * Typing errors
 *)

let type_error pos msg = 
  let as_string f = 
    let b = Buffer.create 13 in 
    let fmt = Format.formatter_of_buffer b in
    f fmt;
    Format.pp_print_flush fmt ();
    Buffer.contents b in
  Error.error "during type checking" pos (as_string msg)

let error_not_equal pos t u =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[%a <> %a.@]@." Print.ptype t Print.ptype u)

let error_not_a_product pos t x =
  type_error pos
    (fun fmt -> Format.fprintf fmt 
       "@[%s cannot be applied to a function of type %a.@]@." x Print.ptype t)

let error_not_a_sort pos t =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[The type of %a is not a sort.@]@." Print.ptype t)

(* 
 * Structural equality modulo α
 * Y: more generally, with respect to a substitution? 
 *)

let rec equal_term sigma a b = 
  match a,b with
    | Var x, Var y -> Subst.lookup sigma x = Subst.lookup sigma y
    | App (a,x), App(b,y) -> 
	Subst.lookup sigma x = Subst.lookup sigma y && equal_term sigma !a !b
    | _ -> false

let rec equal_type env sigma t u = 
  let equal_prod x t1 t2 y u1 u2 = 
    equal_type env sigma !t1 !u1 &&
      let (kx,env) = Env.bind_decl env x !t1 in
      equal_type env (Subst.bind sigma y kx) !t2 !u2
  in
  match t,u with
    | Term a,Term b -> 
	equal_term sigma !a !b
    | Sort KType, Sort KType -> 
	true
    | Prod (x,t1,t2), Prod (y,u1,u2) -> 
	equal_prod x t1 t2 y u1 u2 
    | SProd (x,t1,a,t2), SProd (y,u1,b,u2) ->
	equal_prod x t1 t2 y u1 u2 && equal_term sigma !a !b
    | _ -> 
	false

let check_equal pos sigma t u =
  (* no need to pass the whole env here as keys for open variables
     will be equal *)
  if equal_type Env.empty sigma t u then () else error_not_equal pos t u

(* 
 * Type-checking
 *)

let rec check_term pos env sigma al t = 
  match al,t with
    | _, SProd (y,t,a,u) ->
	let (k,env) = Env.bind_def env !t (Subst.keys_of sigma a) in
	check_term (pos_of u) env (Subst.bind sigma y k) al !u
    | x::al, Prod (y,t,u) ->
	let kx = Subst.lookup sigma x in
	let tx = Env.lookup env kx in
	check_equal (pos_of t) sigma tx !t;
	check_term (pos_of u) env (Subst.bind sigma y kx) al !u
    | [], _ -> t, sigma
    | x::_,_ -> error_not_a_product pos t x

let infer_term pos env sigma a =
  let rec aux pos al = function
    | Var x -> check_term pos env sigma al
	(Env.lookup env (Subst.lookup sigma x))
    | App (a,x) -> aux (pos_of a) (x::al) !a in
  aux pos [] a

let rec infer_type env sigma t =
  match !t with
    | Sort KType -> KType
    | Term a -> 
	(match fst (infer_term (pos_of a) env sigma !a) with
	   | Sort s -> s
	   | _ -> error_not_a_sort (pos_of a) (Term a))
    | Prod (x,t,u) -> 
	ignore (infer_type env sigma t); (* only one sort *)
	let (k,env) = Env.bind_decl env x !t in
	infer_type env (Subst.bind sigma x k) u
    | SProd (x,t,a,u) ->
	ignore (infer_type env sigma t); (* only one sort *)
	let (ta,sigma) = infer_term (pos_of a) env sigma !a in
	check_equal (pos_of t) sigma ta !t;
	let (k,env) = Env.bind_def env !t (Subst.keys_of sigma a) in
	infer_type env (Subst.bind sigma x k) u

