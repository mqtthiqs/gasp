open AST

let ( ! ) = Position.value
let pos_of = Position.position

let prod_rule = function
  | _,s -> s				(* PTS total *)

let axiom_rule = function
  | KType -> KKind
  | _ -> raise Not_found

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

let error_not_equal pos jx jy =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[%a <> %a.@]@." Print.judg jx Print.judg jy)

let error_not_a_product pos a x =
  type_error pos
    (fun fmt -> Format.fprintf fmt 
       "@[%s cannot be applied to %a.@]@." x Print.term a)

let error_not_a_sort pos a =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[The type of %a is not a sort.@]@." Print.term a)

let error_not_bound pos x =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Variable %s is not bound.@]@." x)

let error_prod_rule pos s1 s2 =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Product (%a,%a,_) is not available.@]@." Print.sort s1 Print.sort s2)

let error_type_kind pos s =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[%a has no type.@]@." Print.sort s)

(* 
 * Type-checking
 *)

let has_args e =
  try ignore (Env.pop_decl e); true
  with Env.Empty -> false

let rec equals_term env t1 t2 =
  match t1,t2 with
    | Var x, Var y -> Env.equal env x y 
    | App (a,x), App (b,y) -> Env.equal env x y && equals_term env a b
    | _ -> false

let rec equals_env env e1 e2 =
  try
    let (e1,k1) = Env.pop_decl e1 in
    let (e2,k2) = Env.pop_decl e2 in
    equals_env env e1 e2 &&
      equals_judg env (Env.lookup_key env k1) (Env.lookup_key env k2)
  with Env.Empty -> 
    if has_args e1 || has_args e2 then false else true

and equals_judg env (e1,h1) (e2,h2) =
  equals_env env e1 e2 &&
    match h1,h2 with
      | Term a, Term b -> equals_term env a b
      | Sort s, Sort t -> s=t
      | _ -> false

let rec infer_term pos env : term -> Env.j = function
  | Var x -> 
      ( try Env.lookup env x
	with Not_found -> error_not_bound pos x )
  | App (a,x) ->
      let (ea,ha) = infer_term pos env a in
      Format.printf "app (%a : %a) %s@\n" Print.term a Print.judg (ea,ha) x;
      let (ea,k) = 
	try Env.pop_decl ea
	with Env.Empty -> error_not_a_product pos a x in
      let jx = 
	try Env.lookup env x 
	with Not_found -> error_not_bound pos x in
      let jy = Env.lookup_key ea k in
      if equals_judg env jx jy then
        Env.link ea x k, ha
      else error_not_equal pos jx jy

let rec infer_head pos env = function
  | Sort s ->
      let k = 
	try axiom_rule s
	with Not_found -> error_type_kind pos s in
      (env, Sort s), k
  | Term a ->
      let ja = infer_term pos env a in
      (* Format.printf "app @[%a : %a@]@\n" Print.term a Print.judg ja; *)
      let k = match snd ja, has_args (fst ja) with
	| Sort s, false -> s
	| _ -> 
	    error_not_a_sort pos a in
      (env, Term a), k

let rec infer_type env ty = match !ty with
  | Head h -> 
      infer_head (pos_of ty) env h
  | Prod (x,t,u) -> 
      let (jt,s1) = infer_type (Env.clear_decl env) t in
      Format.printf "intro dec %s : %a@\n" x Print.judg jt;
      let (ju,s2) = infer_type (Env.bind_decl env x jt) u in
      ju, (try prod_rule (s1,s2)
	   with Not_found -> error_prod_rule (pos_of ty) s1 s2)
  | SProd (x,a,u) ->
      Format.printf "typage de (%s = %a)@\n" x Print.term !a;
      let (ea,ha as ja) = infer_term (pos_of a) (Env.clear_decl env) !a in
      Format.printf "intro def %s : %a@\n" x Print.judg ja;
      let (_,s1) = infer_type ea (Position.with_pos (pos_of a) (Head ha)) in
      let (ju,s2) = infer_type (Env.bind_def env x !a ja) u in
      ju, (try prod_rule (s1,s2) 
	    with Not_found -> error_prod_rule (pos_of ty) s1 s2)
