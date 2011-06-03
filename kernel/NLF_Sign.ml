open Name
open NLF

type entry =
  | FDecl of fconst * kind
  | ODecl of oconst * fam

type t = kind Name.Fconstmap.t * fam Name.Oconstmap.t * entry list

let empty = Fconstmap.empty, Oconstmap.empty, []
let add_oconst x a (m, n, l) = m, Oconstmap.add x a n, ODecl (x, a) :: l
let add_fconst x a (m, n, l) = Fconstmap.add x a m, n, FDecl (x, a) :: l
let find_oconst x (_, n, _) = Oconstmap.find x n
let find_fconst x (m, _, _) = Fconstmap.find x m
let mem_oconst x (_, n, _) = Oconstmap.mem x n
let mem_fconst x (m, _, _) = Fconstmap.mem x m

let fold f (_,_,l) acc = List.fold_left (fun acc e -> f e acc) acc l


(* Fconstmap.fold (fun x k acc -> f (FDecl (x,k)) acc) fc *)
(*   (Oconstmap.fold (fun x a acc -> f (ODecl(x,a)) acc) oc acc) *)
