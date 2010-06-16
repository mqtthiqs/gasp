open AST

(** A key for a variable [n] is the hash-code of the term [t] related
    to [n]. We provide a constant time mapping from hash-code to the
    first name used to denote [t] (simply by using a pair as a 
    concrete representation.). Yet, we must keep in mind that this
    type is morally dependent on the environment it comes from. 

    NDY: avec ocaml 3.12, on va pouvoir utiliser les modules de premiere
         classe pour typer fortement tout ca. Youpi :) *)
type key = int * Name.t

(** An environment is responsible for storing data associated to
    hash-codes of terms. For each hash-code, this piece of data is
    composed of a canonical version of the key, a [ptype] and a
    flattened representation of the hashed term. *)
module Env : 
sig

  type t 
  val empty : t
  val lookup : t -> key -> ptype * key list
  val bind_decl : t -> Name.t -> ptype -> (key * t)
  val bind_def  : t -> Name.t -> ptype -> key list -> (key * t)

  (** [canonical_name env k] returns the first name that has been
      associated to data of key [k]. *)
  val canonical_name : t -> key -> Name.t

end = struct
  module Intmap = 
    Map.Make (struct 
		type t = key 
		let compare (k, _) (k', _) = Pervasives.compare k k'
	      end)

  (* ptype are assumed to be small, no hash-consing is done on them. *)
  type t = (key * (ptype * key list)) Intmap.t

  let empty = Intmap.empty

  let lookup env k = snd (Intmap.find k env)

  let canonical_name env k = snd (fst (Intmap.find k env))
    
  let bind env k t =
    try let k' = fst (Intmap.find k env) in k', env
    with Not_found -> k, Intmap.add k (k, t) env
      
  (* TODO être sûr qu'on utilise tous les bits *)
  let bind_decl env x t = bind env (Random.bits(), x) (t, [])
    
  (* Warning: to be correct, the hash function should respect [forall
     a tl, hash(a::tl) = hash(hash a, hash tl)] to ensure that partial
     application is treated correctly (see papp.ga).  Besides, notice
     that for definitions which are just renamings (also called stubs
     in the sequel) like [X = Y], we should have to compute [hash Y]
     and not [hash [hash Y]], but as these two values are equal, 
     there is no issue. *)
  let bind_def env x t a = 
    let ks = fst (List.split a) in
    bind env (Hashtbl.hash ks, x) (t, a)
end

(** A substitution is a mapping from names to keys. Substitutions
    are also used to denote lexical environment, that's why there
    is an implicit ordering between introduced names: there relative
    order is induced by their order in the lexical environment. *)

(* NDY: C'est une propriete tres tres fragile ... A terme, il faudrait
   changer cette structure de donnees. *)
module Subst = struct
  module Idmap = Map.Make (Name)

  type t = key Idmap.t

  let empty = Idmap.empty

  let bind (sigma :t) n (k : key) = Idmap.add n k sigma

  let lookup sigma n = Idmap.find n sigma

  let rec fold_app f acc = function
    | Var x -> f x acc
    | App (a,x) -> f x (fold_app f acc (Position.value a))

  let keys_of sigma a = 
    fold_app (fun x acc -> lookup sigma x :: acc) [] a

  let as_list sigma = 
    List.rev (Idmap.fold (fun k v ks -> (k, v) :: ks) sigma [])

  let to_string sigma = 
    String.concat "\n"
      (List.map (fun (n, (k, _)) -> 
	 "  " ^ Name.to_string_debug n ^ " -> " ^ string_of_int k)
	 (as_list sigma))
		       
end

type t = (Env.t * Subst.t)

let empty = (Env.empty, Subst.empty)

let bind_def ((env,sigma) : t) x a t =
  let (k, env) = Env.bind_def env x t (Subst.keys_of sigma a) in
  (env, Subst.bind sigma x k)

let bind_decl (env,sigma) x t =
  let (k, env) = Env.bind_decl env x t in
  (env, Subst.bind sigma x k)

let lookup (env,sigma) x =
  fst (Env.lookup env (Subst.lookup sigma x))

let lookup_latest_with_prefix ((_, sigma) : t) x = 
  fst (List.find (fun (k, _) -> 
		    Printf.eprintf "=> %s\n%!" (Name.to_string_debug k);
		    Name.has_prefix x k) 
	 (List.rev (Subst.as_list sigma)))

let expand ((env, sigma) : t) on_var on_app x =
  let rec aux x = 
    let _, termkeys = Env.lookup env (Subst.lookup sigma x) in 
    match List.rev termkeys with
      | [] -> on_var x
      | x :: termkeys -> 
	  List.fold_left (fun t n -> on_app t (snd n)) (on_var (snd x)) termkeys
  in
  aux x

let subnames (env, sigma) x = 
  List.map 
    (fun (_, n) -> n) 
    (snd (Env.lookup env (Subst.lookup sigma x)))

let equal (env,sigma) x y =
  fst (Subst.lookup sigma x) = fst (Subst.lookup sigma y)

let lookup_and_bind ((env, sigma) : t) (x : Name.t) y =
  let k = Subst.lookup sigma x in
  let t = Env.lookup env k in
  let sigma = Subst.bind sigma y k in
  (env,sigma), fst t

let export (env : t) n = 
  let wrap = Position.unknown_pos in
  expand env (fun x -> Var x) (fun t1 t2 -> App (wrap t1, t2)) n

let to_ptype ((env, sigma) : t) = 
  let wrap = Position.unknown_pos in
  let cache = Hashtbl.create 13 in
  (* To maintain the maximal sharing of the environment in the
     exported type, we just remember the already met keys and generate
     a stub to the related variables if we meet them again. *)
  let cached (k, _) = Hashtbl.mem cache k in
  let bound (k, n) t = 
    try Hashtbl.find cache k
    with Not_found -> 
      (Hashtbl.add cache k (Var n); t)
  in
  let is_canonical_name env k n = 
    Env.canonical_name env k = n
  in
  let rec aux = function
    | [] -> wrap Cont
    | (n, v) :: bs ->
	let t, subkeys = Env.lookup env v in
	if cached v || (subkeys = [] && not (is_canonical_name env v n)) then 
	  wrap (SProd (n, wrap t, wrap (Var (Env.canonical_name env v)), aux bs))
	else if subkeys = [] then
	  wrap (Prod (n, wrap t, aux bs))
	else 
	  let term = bound v (export (env, sigma) (snd v)) in
	  wrap (SProd (n, wrap t, wrap term, aux bs))
  in
  aux (Subst.as_list sigma)


