let prelude : LF.Sign.t =
  <:sign<
    version : type.
    ancestors : type.
    anil : ancestors.
    acons : version -> ancestors -> ancestors.
  >>
;;

open Util
open Names
open LF

let rec commit_rec repo = function
  | OApp (c, l) when OConstSet.mem c repo.Repo.bound ->
    let repo, l = List.fold_map commit_rec repo l in
    Kernel.push repo (OApp (c, l))
  | m -> repo, m

let commit repo m =
  let x = SLF.Meta(Names.Meta.repr repo.Repo.head) in
  let m = LF.Strat.obj prelude []
    <:obj<
      vcons $m$ (acons $x$ anil)
    >> in
  commit_rec repo m

let merge repo m = assert false
