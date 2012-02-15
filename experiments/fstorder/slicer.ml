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

let commit repo m =
  match LF.Strat.obj repo.Repo.sign [] m with
    | OApp (h, l) -> Kernel.push repo LF.Env.empty (h, l)
    | _ -> failwith "not an app"

let checkout repo =
  LF.Unstrat.obj [] (Kernel.pull repo repo.Repo.head)
