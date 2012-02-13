open Util
open Names
open LF

let prelude : LF.Sign.t = Kernel.init LF.Sign.empty
  <:sign<
    (* version : type. *)
    (* ancestors : type. *)
    (* anil : ancestors. *)
    (* acons : version -> ancestors -> ancestors. *)
  >>

let init sign : Repo.t = {
  Repo.sign = Kernel.init prelude sign;
  Repo.ctx = Repo.Context.empty;
  Repo.head = Meta.make "DUMMY";
}

let rec slice repo m =
  Format.printf "** slice %a@." LF.Printer.obj m;
  match m with
  | OLam (x, m) ->
    let repo, m = slice repo m in
    repo, OLam (x, m)
  | OApp (h, l) ->
    let repo, l = List.fold_map slice repo l in
    begin match h with
      | HConst c when Sign.slices c repo.Repo.sign ->
        let repo = Kernel.push repo (h, l) in
        repo, OMeta (repo.Repo.head)
      | h -> repo, OApp (h, l)
    end
  | OMeta _ as m -> repo, m

let commit repo m =
  Format.printf "*** commit %a@." SLF.Printer.term m;
  match LF.Strat.obj repo.Repo.sign [] m with
    | OApp (h, l) ->
      let repo, m = slice repo (OApp (h, l)) in
      Kernel.push repo (h, l)
    | _ -> failwith "not an app"
