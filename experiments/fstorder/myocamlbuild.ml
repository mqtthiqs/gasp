open Ocamlbuild_plugin
open Command
;;

dispatch begin function
  | After_rules ->
    flag ["ppopt_SLF"] & S[A"-ppopt"; P"SLF.cmo"]
  | _ -> ()
end
