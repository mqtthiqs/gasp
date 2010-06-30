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
       "@[%a cannot be applied to a function of type %a.@]@." Print.id x Print.ptype t)

let error_not_a_sort pos t =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[The type of %a is not a sort.@]@." Print.ptype t)

let error_not_bound pos x =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Variable %a is not bound.@]@." Print.id x)

let error_prod_rule pos s1 s2 =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Product (%a,%a,_) is not available.@]@." Print.sort s1 Print.sort s2)

let error_type_kind pos s =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[%a has no type.@]@." Print.sort s)

(* 
 * Structural equality modulo α
 * Y: more generally, with respect to a substitution? 
 *)

let rec equal_term env a b = 
  match a,b with
    | Var x, Var y -> Env.equal env x y
    | App (a,x), App(b,y) -> 
	Env.equal env x y && equal_term env !a !b
    | _ -> false

let rec equal_type env t u = 
  let equal_prod x t1 t2 y u1 u2 = 
    equal_type env !t1 !u1 &&
      let env = Env.bind_decl env x !t1 in
      equal_type env !t2 !u2 in
  match t,u with
    | Term a,Term b -> 
	equal_term env !a !b
    | Sort s1, Sort s2 -> s1 = s2
    | Prod (x,t1,t2), Prod (y,u1,u2) -> 
	equal_prod x t1 t2 y u1 u2 
    | SProd (x,t1,a,t2), SProd (y,u1,b,u2) ->
	equal_prod x t1 t2 y u1 u2 && equal_term env !a !b
    | _ -> 
	false

let check_equal pos env t u =
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
  (* TODO: clear l'env dans env pour être plus efficace *)
  if equal_type env t u then () else error_not_equal pos t u

(* 
 * Type-checking
 *)

(* Are [x1, ..., xn] correct formal arguments for a function of
   type [P1. ... PM. T] where P is a product binder [(y : t).] or a
   bounded product binder [(y : t = a).] under a given substition
   sigma? *)
let rec check_term pos env al t = 
  match al,t with
    | _, SProd (y,t,a,u) ->
	(* If there is an object "a" in the store, we can use its
	   internal name as a concrete name for [y] without consuming
	   a formal argument. 
	   First, this is enforcing the invariant of (syntactic, not
	   semantic) maximal sharing. Second, this is avoiding an
	   explicit substitution of [a] for [y] in [u]. *)
	let env = Env.bind_def env y !a !t in
	check_term pos env al !u
    | x::al, Prod (y,t,u) ->
	let (env',tx) = 
	  try Env.lookup_and_bind env x y
	  with Not_found -> error_not_bound pos x in
	check_equal pos env tx !t;
	check_term pos env' al !u
    | [], _ -> 
	(* Partial application is allowed. *)
	t, env
    | x::_,_ -> 
	(* Too many formal arguments for this function type. *)
	error_not_a_product pos t x

(* Is a sequence of application [F x1 ... xn] well-formed?  *)
let infer_term pos env a =
  let rec aux al = function
    | Var x -> 
	(try check_term pos env al (Env.lookup env x)
	 with Not_found -> error_not_bound pos x)
    | App (a,x) -> 
	aux (x::al) !a 
    | Unknown -> assert false (* FIXME *)
  in
  aux [] a

let prod_rule = function
  | _,s -> s				(* PTS total *)

let axiom_rule = function
  | KType -> KKind
  | _ -> raise Not_found

(* Check that [t] is well-formed under the substitution [sigma] and 
   the repos*)
type inference_result =
    RSort of sort   (* For a closed type. *)
  | REnv  of Env.t  (* For a context.     *)

exception IsNotClosed
exception IsClosed

let rec infer env ty : inference_result =
  match !ty with
    | Sort s ->
	(try RSort (axiom_rule s)
	 with Not_found -> error_type_kind (pos_of ty) s)
    | Term a ->
	(match fst (infer_term (pos_of a) env !a) with
	   | Sort s -> RSort s
	   | _ -> error_not_a_sort (pos_of a) (Term a))
    | Prod (x,t,u) -> 
	let s1 = infer_type env t in
	let env = Env.bind_decl env x !t in
	let s2 = infer env u in
	(match s2 with
	   | REnv _ -> s2 
	   | RSort s2 -> 
	       (try RSort (prod_rule (s1, s2))
		with Not_found -> error_prod_rule (pos_of ty) s1 s2))
    | SProd (x,t,a,u) ->
	let s1 = infer_type env t in
	(let (ta,env) = infer_term (pos_of a) env !a in
	 check_equal (pos_of t) env ta !t); (* parenthesis are important: 
						 env is only used to check 
						 equality, not for the rest *)
	let env = Env.bind_def env x !a !t in
	(match infer env u with
	   | RSort s2 -> 
	       (try 	
		  RSort (prod_rule (s1,s2))
		with Not_found -> error_prod_rule (pos_of ty) s1 s2)
	   | r -> r)
    | Cont ->
	REnv env

and infer_type env t = 
  match infer env t with
    | REnv _  -> raise IsNotClosed
    | RSort s -> s

and infer_env env t = 
  match infer env t with
    | RSort _  -> raise IsClosed
    | REnv e -> e
