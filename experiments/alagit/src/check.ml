open AST

let ( ! ) = Position.value

let type_error pos = 
  Error.error "during type checking" pos "Error."

let rec term_subst x y = function
  | Var z      when z = x -> Var y
  | App (a, z) when z = x -> App (term_subst' x y a, y)
  | App (a, z)            -> App (term_subst' x y a, z)
  | x			  -> x

and term_subst' x y = Position.map (term_subst x y) 

let rec ptype_subst x y = function
  | Term a -> 
      Term (term_subst' x y a)
  | Prod (z, t, s) when z = x -> 
      Prod (z, ptype_subst' x y t, s)
  | SProd (z, t, a, s) when z = x -> 
      SProd (z, ptype_subst' x y t, term_subst' x y a, s)
  | Prod (z, t, s) -> 
      Prod (z, ptype_subst' x y t, ptype_subst' x y s)
  | SProd (z, t, a, s) -> 
      SProd (z, ptype_subst' x y t, term_subst' x y a, ptype_subst' x y s)
  | x ->
      x

and ptype_subst' x y = Position.map (ptype_subst x y) 
      
let rec term_equal t1 t2 = 
  match t1, t2 with
    | Var x, Var y -> 
	x = y
    | App (t1, x1), App (t2, x2) -> 
	term_equal' t1 t2 && x1 = x2
    | _ -> 
	false

and term_equal' t1 t2 = 
  term_equal (! t1) (! t2)

let rec ptype_equal t1 t2 = 
  match t1, t2 with
    | Term t1, Term t2 -> 
	term_equal' t1 t2
    | Sort s1, Sort s2 -> 
	s1 = s2
    | Prod (x1, t1, s1), Prod (x2, t2, s2) -> 
	ptype_equal' t1 t2 && ptype_equal' s1 (ptype_subst' x2 x1 s2)
    | SProd (x1, t1, a1, s1), SProd (x2, t2, a2, s2) -> 
	ptype_equal' t1 t2 
	&& ptype_equal' s1 (ptype_subst' x2 x1 s2)
	&& term_equal' a1 a2
    | _ ->
	false

and ptype_equal' t1 t2 = 
  ptype_equal (! t1) (! t2)


module Env : sig
  type t
  val type_lookup : Position.t -> t -> id -> ptype
  val def_lookup  : Position.t -> t -> id -> ptype * term
  val empty : t
  val bind_def : Position.t -> t -> id -> ptype -> term -> t
  val bind_dec : Position.t -> t -> id -> ptype -> t
end = struct
  (* FIXME: use a functional hashtbl instead. *)
  type t = (id * entry) list

  let empty = 
    []

  let bind_def pos env x ty d = 
    if (List.mem_assoc x env) then type_error pos;
    (x, Def (ty, d)) :: env

  let bind_dec pos env x ty = 
    if (List.mem_assoc x env) then type_error pos;
    (x, Dec ty) :: env

  let lookup pos env x = 
    try 
      List.assoc x env
    with Not_found -> 
      type_error pos

  let type_lookup pos env x = 
    match lookup pos env x with
      | Def (t, _) -> t
      | Dec t      -> t

  let def_lookup pos env x =
    match lookup pos env x with
      | Def (s, d) -> (s, d)
      | _ -> type_error pos
end

let rec check_term pos env ptype = function
  | Var x -> 
      let x_ptype = Env.type_lookup pos env x in 
      check_ptype_equal pos x_ptype ptype
  | App (a, x) ->
      let a_ptype   = infer_term' env a in
      let app_ptype = infer_application_ptype pos env a_ptype x in
      check_ptype_equal pos app_ptype ptype

and check_term' env ptype a = check_term (Position.position a) env ptype !a

and infer_term pos env = function
  | Var x -> 
      Env.type_lookup pos env x 
  | App (a, x) ->
      let a_ptype   = infer_term' env a in
      infer_application_ptype pos env a_ptype x

and infer_term' env term' = infer_term (Position.position term') env !term'
	
and infer_application_ptype pos env a_ptype x = 
  let (y, s, t, x_ptype) =
    match a_ptype with
    | Prod (y, t, s) ->
	(* FIXME: Choose [y] fresh for env? *)
	(y, s, t, Env.type_lookup pos env x)
    | SProd (y, t, a, s) ->
	let (x_ptype, x_def) = Env.def_lookup pos env x in
	check_term_equal pos x_def (! a);
	(y, s, t, x_ptype)
    | _ -> 
	type_error pos
  in
  check_ptype_equal pos (! t) x_ptype;
  ! (ptype_subst' y x s)
	    
and check_ptype pos env ptype = function
  | Term t -> 
      check_term pos env ptype !t
  | Sort s -> 
      ()
  | Prod (y, t, s) -> 
      check_ptype' env (Sort KType) t;
      check_ptype' (Env.bind_dec pos env y !t) (Sort KType) s
  | SProd (y, t, a, s) -> 
      check_ptype' env (Sort KType) t;
      check_term' env !t a;
      check_ptype' (Env.bind_def pos env y !t !a) (Sort KType) s

and check_ptype' env ptype ptype' = 
  check_ptype (Position.position ptype') env ptype (Position.value ptype')

and check_ptype_equal pos t1 t2 = 
  if not (ptype_equal t1 t2) then 
    type_error pos 

and check_term_equal pos t1 t2 = 
  if not (term_equal t1 t2) then 
    type_error pos 
  
let patch (Patch s) = 
  check_ptype' Env.empty (Sort KType) s
