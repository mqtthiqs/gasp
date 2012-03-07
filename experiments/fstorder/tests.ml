open SLF
open Struct
open Struct.Repo

let commit repo m =
  let repo = Slicer.commit repo Env.empty m in
  let _, _, a = Context.find repo.head repo.ctx in
  let m = Strat.obj repo.sign [] m in
  let n = Strat.obj repo.sign [] (Slicer.checkout repo) in
  Kernel.Conv.obj repo Env.empty (m, n, a);
  repo

let commit_eq repo m p =
  let repo = Slicer.commit repo Env.empty m in
  let _, _, a = Context.find repo.head repo.ctx in
  let m = Strat.obj repo.sign [] m in
  let p = Strat.obj repo.sign [] p in
  let n = Strat.obj repo.sign [] (Slicer.checkout repo) in
  Kernel.Conv.obj repo Env.empty (m, n, a);
  Kernel.Conv.obj repo Env.empty (m, p, a);
  repo

(* ([x] n) [p <- m] *)
let subst repo m n p a =
  let p = SLF.Strat.obj repo.Struct.Repo.sign [] p in
  let m = SLF.Strat.obj repo.Struct.Repo.sign [] m in
  let a = SLF.Strat.fam repo.Struct.Repo.sign [] a in
  let n = match LF.prj (SLF.Strat.obj repo.Struct.Repo.sign [] n) with
    | LF.OLam (_, n) -> n
    | _ -> assert false in
  let q = LF.Subst.obj [m] n in
  Kernel.Conv.obj repo Env.empty (p, q, a);
  p
