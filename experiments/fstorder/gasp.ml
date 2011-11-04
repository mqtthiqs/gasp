let _ =
  print_string "Gasp!\n";
  ()

let repo = ref None

let init f = repo := Some (Repo.init Repo.Sign.empty)       (* TODO *)
let load f = repo := Some (Repo.load f)
let save f = match !repo with
  | None -> failwith "empty repo"
  | Some repo -> Repo.save f repo

open Kernel
