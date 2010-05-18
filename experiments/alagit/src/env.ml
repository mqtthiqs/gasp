open AST

type key = int
module Intmap = Map.Make(struct type t = key let compare = Pervasives.compare end)
type t = ptype Intmap.t
let empty = Intmap.empty

let lookup env k = try Intmap.find k env with Not_found -> failwith ("not found "^string_of_int k) (* DEBUG *)
  
(* TODO Comparer les types *)
let bind_decl env n t = 
  let k = Hashtbl.hash n in
  try ignore (Intmap.find k env); failwith (n^" already in the env")
  with Not_found -> k, Intmap.add k t env
    
let bind_def env t a =
  let k = Hashtbl.hash a in
  try ignore (Intmap.find k env); k, env
  with Not_found -> k, Intmap.add k t env
