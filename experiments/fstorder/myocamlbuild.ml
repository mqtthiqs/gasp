open Ocamlbuild_plugin
open Command
;;

dispatch begin function
  | After_options ->
      pflag ["ocaml"; "compile"] "warn" (fun s -> S[A"-w"; A s]);
      pflag_and_dep ["ocaml"; "ocamldep"] "pa" (fun pa -> S[A"-ppopt"; P ("pa_"^pa^".cma")]);
      pflag_and_dep ["ocaml"; "compile"] "pa" (fun pa -> S[A"-ppopt"; P ("pa_"^pa^".cma")]);
      pdep [] "dep" (fun d -> [d]);
      rule ".log" ~prod: "%.log" ~dep: "%.byte" (fun env _ ->
        let byte = env "%.byte" in let log = env "%.log" in
                                   Cmd(Sh ("./"^byte^" > "^log))
      )
  | _ -> ()
end
