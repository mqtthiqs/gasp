open Name
open NLF

type t = kind Name.Fconstmap.t * fam Name.Oconstmap.t

let fold f (fc,oc) acc = Fconstmap.fold (fun x k acc -> f (FDecl (x,k)) acc) fc
  (Oconstmap.fold (fun x a acc -> f (ODecl(x,a)) acc) oc acc)
let empty = Fconstmap.empty, Oconstmap.empty
