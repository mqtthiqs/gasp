open AST

type key = int

module Keymap = Map.Make(struct type t = key let compare = Pervasives.compare end)

type head = 
  | Hsort of sort
  | Happ of head * key
  
type t = {
  env : j Keymap.t;
  free : key list
}
and j = t * head
    
let empty = {
    env = Keymap.empty; 
  free = [];
}
  
let lookup env k = 
  Keymap.find k env.env

let bind_def env l j = 
  let k = Hashtbl.hash l in
  { env = Keymap.add k j env.env;
    free = k :: env.free }, k

let bind_decl env j =
  let k = Random.bits() in
  { env with env = Keymap.add k j env.env }, k

exception Empty

let list_last = 
  let rec aux l = 
    function
      | [] -> raise Empty
      | [a] -> a, List.rev l
      | a::tl -> aux (a::l) tl in
  aux []

let pop_decl env = 
  let (x,f) = list_last env.free in
  {env with free = f}, x

let clear_decl env = { env with free=[] }


(* module Idmap = Map.Make(struct type t = id let compare = Pervasives.compare end) *)

(* let keys_of sigma a =  *)
(*   let rec fold_app f acc = function *)
(*     | Var x -> f x acc *)
(*     | App (a,x) -> f x (fold_app f acc a) in *)
(*   fold_app (fun x acc -> Idmap.find x sigma::acc) [] a *)

(* let bind_def env x a t = *)
(*   let k = Hashtbl.hash (keys_of env.sigma a) in *)
(*   { env with  *)
(*       env = Keymap.add k t env.env;  *)
(*       sigma = Idmap.add x k env.sigma } *)

(* let bind_decl env x t = *)
(*   let k = Random.bits() in *)
(*   { env = Keymap.add k t env.env; *)
(*     sigma = Idmap.add x k env.sigma; *)
(*     free = x :: env.free } *)

(* let lookup_key env k = Keymap.find k env.env *)

(* let lookup env x = *)
(*   lookup_key env (Idmap.find x env.sigma)  *)

(* let link env x y = *)
(*   { env with sigma = Idmap.add x (Idmap.find y env.sigma) env.sigma} *)

(* let equal e1 e2 x y = *)
(*   Idmap.find x e1.sigma = Idmap.find y e2.sigma *)


