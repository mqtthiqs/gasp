open Ocamlbuild_plugin
open Command
;;

dispatch begin function
  | After_options ->
    pflag_and_dep ["ocaml"; "ocamldep"] "pa" (fun pa -> S[A"-ppopt"; P pa]);
    pflag_and_dep ["ocaml"; "compile"] "pa" (fun pa -> S[A"-ppopt"; P pa]);
  | _ -> ()
end
