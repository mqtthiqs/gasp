open AST

type key = int

module Env = struct
  module Intmap = Map.Make (struct type t = key let compare = Pervasives.compare end)

  type t = ptype Intmap.t

  let empty = Intmap.empty

  let lookup env k = Intmap.find k env
    
  let bind env k t =
    try ignore (Intmap.find k env); k,env
    with Not_found -> k, Intmap.add k t env
      
  (* TODO être sûr qu'on utilise tous les bits *)
  let bind_decl env t = bind env (Random.bits()) t
    
  (* Warning: to be correct, the hash function should respect [forall a
     tl, hash(a::tl) = hash(hash a, hash tl)] to ensure that partial
     application is treated correctly (see papp.ga) *)
  let bind_def env t a = bind env (Hashtbl.hash a) t
end

module Subst = struct
  module Idmap = Map.Make (Name)

  type t = key Idmap.t

  let empty = Idmap.empty

  let bind sigma n k = Idmap.add n k sigma

  let lookup sigma n = Idmap.find n sigma

  let rec fold_app f acc = function
    | Var x -> f x acc
    | App (a,x) -> f x (fold_app f acc (Position.value a))

  let keys_of sigma a = 
    fold_app (fun x acc -> lookup sigma x :: acc) [] a

  let as_list sigma = 
    List.rev (Idmap.fold (fun k v ks -> (k, v) :: ks) sigma [])
end

type t = (Env.t * Subst.t)

let empty = (Env.empty, Subst.empty)

let bind_def (env,sigma) x a t =
  let (k,env) = Env.bind_def env t (Subst.keys_of sigma a) in
  (env, Subst.bind sigma x k)

let bind_decl (env,sigma) x t =
  let (k,env) = Env.bind_decl env t in
  (env, Subst.bind sigma x k)

let lookup (env,sigma) x =
  Env.lookup env (Subst.lookup sigma x)

let equal (env,sigma) x y =
  Subst.lookup sigma x = Subst.lookup sigma y

let lookup_and_bind (env,sigma) x y =
  let k = Subst.lookup sigma x in
  let t = Env.lookup env k in
  let sigma = Subst.bind sigma y k in
  (env,sigma), t

let to_ptype (env, sigma) = 
  let wrap = Position.unknown_pos in
  List.fold_left 
    (fun c (k, v) -> 
       let t = wrap (Env.lookup env v) in
       wrap (Prod (k, t, c)))
    (wrap Cont)
    (Subst.as_list sigma)
