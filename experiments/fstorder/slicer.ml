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
  Repo.head = Meta.make "DUMMY";
}

let rec slice repo = function
  | OApp (c, l) ->
    let repo, l = List.fold_map slice repo l in
    if Sign.slices c repo.Repo.sign
    then
      let repo = Kernel.push repo (OApp (c, l)) in
      repo, OMeta (repo.Repo.head)
    else repo, OApp (c, l)
  | m -> repo, m

let commit repo m =
  (* Format.printf "*** commit %a@." SLF.Printer.term m; *)
  let m = LF.Strat.obj repo.Repo.sign [] m in
  let repo, m = slice repo m in
  Kernel.push repo m

let merge repo m = assert false
