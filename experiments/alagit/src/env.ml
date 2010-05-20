open AST

type key = int
module Intmap = Map.Make(struct type t = key let compare = Pervasives.compare end)
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
