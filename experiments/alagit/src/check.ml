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

let error_not_bound pos x =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Variable %s is not bound.@]@." x)

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
      let (kx,env) = Env.bind_decl env !t1 in
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
  (* Y: What are "open variables"? *)
  (* Y: For the moment, I am not totally convinced that syntactic
   * equality is enough for our purpose. In the interpretation of our
   * types, I think that the symbol '=' should mean "provably equal"
   * (with respect to the Coq underlying implementation of the
   * metatheory). So, maybe we should add the possibility to decide if
   * (F (X1, ..., XN) = G (Y1, ..., YM)) (efficiently using
   * hash-consing for instance).  In practical terms, this means that
   * each type that represents object-level term should be equipped
   * with a decidable equality. 
   * I don't know if this makes any sense but this idea is haunting my 
   * mind for several days now. 
   *)
  if equal_type Env.empty sigma t u then () else error_not_equal pos t u

(* 
 * Type-checking
 *)

(* Are [x1, ..., xn] correct formal arguments for a function of
   type [P1. ... PM. T] where P is a product binder [(y : t).] or a
   bounded product binder [(y : t = a).] under a given substition
   sigma? *)
let rec check_term pos env sigma al t = 
  match al,t with
    | _, SProd (y,t,a,u) ->
	(* If there is an object "a" in the store, we can use its
	   internal name as a concrete name for [y] without consuming
	   a formal argument. 
	   First, this is enforcing the invariant of (syntactic, not
	   semantic) maximal sharing. Second, this is avoiding an
	   explicit substitution of [a] for [y] in [u]. *)
	let (k,env) = Env.bind_def env !t (Subst.keys_of sigma a) in
	check_term pos env (Subst.bind sigma y k) al !u
    | x::al, Prod (y,t,u) ->
	let kx = 
	  try Subst.lookup sigma x 
	  with Not_found -> error_not_bound pos x in
	let tx = Env.lookup env kx in
	check_equal pos sigma tx !t;
	check_term pos env (Subst.bind sigma y kx) al !u
    | [], _ -> 
	(* Partial application is allowed. *)
	t, sigma
    | x::_,_ -> 
	(* Too many formal arguments for this function type. *)
	error_not_a_product pos t x

(* Is a sequence of application [F x1 ... xn] well-formed?  *)
let infer_term pos env sigma a =
  let rec aux pos al = function
    | Var x -> 
	let kx = 
	  try Subst.lookup sigma x 
	  with Not_found -> error_not_bound pos x in
	check_term pos env sigma al (Env.lookup env kx)
    | App (a,x) -> 
	aux (pos_of a) (x::al) !a in
  aux pos [] a

(* Check that [t] is well-formed under the substitution [sigma] and 
   the repos*)
let rec infer_type env sigma t =
  match !t with
    | Sort KType -> KType
    | Term a -> 
	(match fst (infer_term (pos_of a) env sigma !a) with
	   | Sort s -> s
	   | _ -> error_not_a_sort (pos_of a) (Term a))
    | Prod (x,t,u) -> 
	ignore (infer_type env sigma t); (* only one sort *)
	let (k,env) = Env.bind_decl env !t in
	infer_type env (Subst.bind sigma x k) u
    | SProd (x,t,a,u) ->
	ignore (infer_type env sigma t); (* only one sort *)
	let (ta,sigma) = infer_term (pos_of a) env sigma !a in
	check_equal (pos_of t) sigma ta !t;
	let (k,env) = Env.bind_def env !t (Subst.keys_of sigma a) in
	infer_type env (Subst.bind sigma x k) u
