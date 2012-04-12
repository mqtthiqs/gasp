open Util
open Names
open LF
open Struct

let prelude : repo = Kernel.init Repo.empty
  <:sign<
    (* version : type. *)
    (* ancestors : type. *)
    (* anil : ancestors. *)
    (* acons : version -> ancestors -> ancestors. *)
  >>

let init sign : repo = Kernel.init prelude sign

let commit repo env = SLF.Strat.obj repo.Repo.sign (Env.names_of env) @> prj @> function
    | OApp (h, l) -> Kernel.push repo env (h, l)
    | _ -> failwith "not an app"

let checkout repo =
  SLF.Unstrat.obj [] (Kernel.pull repo repo.Repo.head)
