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
    let init_sign = Parsers.parse_sign !Settings.init_file in
    let sign = Parsers.parse_sign sign_file in
    let repo = Repo.init (init_sign @ sign) in
    Repo.save repo

  | ["commit"; term_file] ->
    let term = Parsers.parse_term term_file in
    let repo = Repo.commit (Repo.load ()) term in
    Repo.save repo

  | ["checkout"] -> Repo.checkout (Repo.load())
  | ["show"] -> Repo.show (Repo.load ())
  | ["check"] -> Repo.check (Repo.load ())

  | _ -> Arg.usage options usage_msg; exit(1)
      
let _ =
  let args = ref [] in
  Arg.parse options (fun f -> args := f :: !args) usage_msg;
  parse_args (List.rev !args)
