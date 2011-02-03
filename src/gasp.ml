open Pp

let options = Arg.align
  [
    "--debug", Arg.Set Settings.debug, "Set debug mode.";
    "--repo", Arg.String (fun x -> Settings.repo := x), "The path of the repository file"
  ]

let usage_msg =
  Printf.sprintf ("%s [command] ...\n")
    (Filename.basename Sys.executable_name)

let parse_args = function
  | ["init"; sign_file]  -> 
      let sign = Parsers.parse_sign sign_file in
      let repo = Repo.init sign in
      Repo.save repo

  | ["commit"; term_file] ->
      let term = Parsers.parse_term term_file in
      let repo = Repo.compile (Repo.load ()) term in
      Repo.save repo

  | ["show"] ->
      Repo.show (Repo.load ())

  | _ -> print_string usage_msg; exit(1)
      
let _ =
  let args = ref [] in
  Arg.parse options (fun f -> args := f :: !args) usage_msg;
  parse_args (List.rev !args)
