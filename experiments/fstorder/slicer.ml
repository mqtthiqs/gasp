let prelude = LF.Strat.sign LF.Sign.empty
  <:raw_sign<
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
  let m = OApp (OConst.make "vcons",
            [m; OApp (OConst.make "acons",
                      [OMeta repo.Repo.head; OApp (OConst.make "anil", [])])
                                 ]) in
  commit_rec repo m

let merge repo m = assert false
