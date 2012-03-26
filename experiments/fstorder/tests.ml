open SLF
open Struct
open Struct.Repo

let commit repo m =
  let repo = Slicer.commit repo [] m in
  let _, _, a = Context.find repo.head repo.ctx in
  let m = Strat.obj repo.sign [] m in
  let n = Strat.obj repo.sign [] (Slicer.checkout repo) in
  Kernel.Conv.obj repo [] (m, n, a);
  repo

let commit_eq repo m p =
  let repo = Slicer.commit repo [] m in
  let _, _, a = Context.find repo.head repo.ctx in
  let m = Strat.obj repo.sign [] m in
  let p = Strat.obj repo.sign [] p in
  let n = Strat.obj repo.sign [] (Slicer.checkout repo) in
  Kernel.Conv.obj repo [] (m, n, a);
  Kernel.Conv.obj repo [] (m, p, a);
  repo

(* ([x] n) [p <- m] *)
let subst repo m n p a =
  let p = SLF.Strat.obj repo.sign [] p in
  let m = SLF.Strat.obj repo.sign [] m in
  let a = SLF.Strat.fam repo.sign [] a in
  let n = match LF.prj (SLF.Strat.obj repo.sign [] n) with
    | LF.OLam (_, n) -> n
    | _ -> assert false in
  let q = LF.Subst.obj [m] n in
  Kernel.Conv.obj repo [] (p, q, a);
  p

let conv repo m n a =
  let m = SLF.Strat.obj repo.sign [] m in
  let n = SLF.Strat.obj repo.sign [] n in
  let a = SLF.Strat.fam repo.sign [] a in
  Kernel.Conv.obj repo [] (m, n, a)

exception Did_not_fail
exception Failed of exn

let fail1 f x =
  try ignore (f x); raise Did_not_fail
  with
    |  Did_not_fail -> raise Did_not_fail
    | e -> raise (Failed e)

let fail2 f x y =
  try ignore (f x y); raise Did_not_fail
  with
    |  Did_not_fail -> raise Did_not_fail
    | e -> raise (Failed e)

let fail3 f x y z =
  try ignore (f x y z); raise Did_not_fail
  with
    |  Did_not_fail -> raise Did_not_fail
    | e -> raise (Failed e)

let fail4 f x y z t =
  try ignore (f x y z t); raise Did_not_fail
  with
    |  Did_not_fail -> raise Did_not_fail
    | e -> raise (Failed e)

let fail5 f x y z t u =
  try ignore (f x y z t u); raise Did_not_fail
  with
    |  Did_not_fail -> raise Did_not_fail
    | e -> raise (Failed e)

