open Ocamlbuild_plugin
open Command
;;

dispatch begin function
  | After_options ->
    pflag_and_dep ["ocaml"; "ocamldep"] "pa" (fun pa -> S[A"-ppopt"; P pa]);
    pflag_and_dep ["ocaml"; "compile"] "pa" (fun pa -> S[A"-ppopt"; P pa]);
    pdep [] "dep" (fun d -> [d]);
    rule ".log" ~prod: "%.log" ~dep: "%.byte" (fun env _ ->
      let byte = env "%.byte" in let log = env "%.log" in
      Cmd(Sh ("./"^byte^" > "^log))
    )
  | _ -> ()
end
