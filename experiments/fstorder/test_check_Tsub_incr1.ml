#use "load.ml"
;;

(* Tsub: Incremental tests 1 *)

open Test_check_Tsub

let repo = Tests.commit repo
<<
  infer (lam nat [x] s x)
>>
;;
