open AST

type key = int

module Intmap = Map.Make(struct type t = key let compare = Pervasives.compare end)
module Idmap = Map.Make(struct type t = id let compare = Pervasives.compare end)

type t = {
  env : j Intmap.t;
  sigma : key Idmap.t;
  free : key list
}
and j = t * head

let empty = {
  env = Intmap.empty; 
  sigma = Idmap.empty;
  free = [];
}

let keys_of sigma a = 
  let rec fold_app f acc = function
    | Var x -> f x acc
    | App (a,x) -> f x (fold_app f acc a) in
  fold_app (fun x acc -> Idmap.find x sigma::acc) [] a

let bind_def env x a t =
  let k = Hashtbl.hash (keys_of env.sigma a) in
  { env with 
      env = Intmap.add k t env.env; 
      sigma = Idmap.add x k env.sigma }

let bind_decl env x t =
  let k = Random.bits() in
  { env = Intmap.add k t env.env;
    sigma = Idmap.add x k env.sigma;
    free = k :: env.free }

let lookup_key env k = Intmap.find k env.env

let lookup env x =
  lookup_key env (Idmap.find x env.sigma) 

let link env x k =
  { env with sigma = Idmap.add x k env.sigma}

let equal env x y =
  Idmap.find x env.sigma = Idmap.find y env.sigma

let lookup_and_link env x y =
  let k = Idmap.find x env.sigma in
  let t = Intmap.find k env.env in
  { env with sigma = Idmap.add y k env.sigma }, t

exception Empty

let list_last = 
  let rec aux l = 
  function
  | [] -> raise Empty
  | [a] -> a, List.rev l
  | a::tl -> aux (a::l) tl in
  aux []

let pop_decl env = 
  let (k,f) = list_last env.free in
  {env with free = f}, k

let clear_decl env = { env with free=[] }
