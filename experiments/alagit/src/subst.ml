open AST

let (!) = Position.value

module Idmap = Map.Make(struct type t = id let compare = Pervasives.compare end)

type t = Env.key Idmap.t

let empty = Idmap.empty

let bind sigma n k = 
  (* Printf.printf "subst: binding %s to %d\n" n k; *)
  Idmap.add n k sigma

let lookup sigma n = try Idmap.find n sigma with Not_found -> failwith ("not found "^n) (* DEBUG *)

let fold = Idmap.fold

let rec fold_app f acc = function
  | Var x -> f x acc
  | App (a,x) -> f x (fold_app f acc !a)

let keys_of sigma a = fold_app (fun x acc -> lookup sigma x::acc) [] !a
