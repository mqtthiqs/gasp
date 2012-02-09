open Util
open Names
open LF

let prelude : LF.Sign.t = Kernel.init LF.Sign.empty
  <:sign<
    version : type.
    ancestors : type.
    anil : ancestors.
    acons : version -> ancestors -> ancestors.
  >>

let init sign : Repo.t = {
  Repo.sign = Kernel.init prelude sign;
  Repo.ctx = Repo.Context.empty;
  Repo.head = Meta.make "empty";
  Repo.bound = OConstSet.empty
}

let rec commit_rec repo = function
  | OApp (c, l) when OConstSet.mem c repo.Repo.bound ->
    let repo, l = List.fold_map commit_rec repo l in
    let repo, x = Kernel.push repo (OApp (c, l)) in
    repo, OMeta x
  | m -> repo, m

let commit repo m =
  let m = LF.Strat.obj repo.Repo.sign [] m in
  let repo, m = commit_rec repo m in
  let repo, x = Kernel.push repo m in
  repo, x

let merge repo m = assert false
