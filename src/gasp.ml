let (_ : unit) =
  MonoLam.init ()

let options = Arg.align
  [
    "--debug", Arg.Set Settings.debug, 
    " Set debug mode.";
  ]

let usage_msg =
  Printf.sprintf "%s:" (Filename.basename Sys.executable_name)

let filenames =
  let filenames = ref [] in
    Arg.parse options (fun f -> filenames := f :: !filenames) usage_msg;
    !filenames

let _ =
  ()
