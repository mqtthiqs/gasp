open Util
open Names
open LF

let prelude : Repo.t = Kernel.init Repo.empty
  <:sign<
    (* version : type. *)
    (* ancestors : type. *)
    (* anil : ancestors. *)
    (* acons : version -> ancestors -> ancestors. *)
  >>

let init sign : Repo.t = Kernel.init prelude sign

let commit repo m =
  match LF.Strat.obj repo.Repo.sign [] m with
    | OApp (h, l) -> fst (Kernel.push repo LF.Env.empty (h, l))
    | _ -> failwith "not an app"

let checkout repo =
  LF.Unstrat.obj [] (Kernel.pull repo repo.Repo.head)
