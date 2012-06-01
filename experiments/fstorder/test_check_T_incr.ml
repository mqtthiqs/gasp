#use "load.ml"
#load "test_check_T.cmo"
;;

(* Test T: batch tests *)

open Test_check_T
;;

(* Incremental tests *)

let repo = Tests.commit repo
<<
  infer (lam nat [x] s x)
>>
;;

let repo = Tests.commit repo
<<
  infer (lam nat [x] infer^0 x ?X4[x; prj x nat (infer x)])
>>
;;

(* let repo = Tests.commit repo *)
(* << *)
(*   infer (recb (s o) (s o) [x] [y] s x) *)
(* >> *)
(* ;; *)

(* let repo = Tests.commit repo *)
(* << *)
(*   infer ( *)
(*     letb (lam nat [x] lam nat [y] recb x y [z] [_] (infer^0 z ?X26[z;infer z])) [add] *)
(*     app (app add (infer^0 ?X23) (infer^0 ?X23)) *)
(*   ) *)
(* >> *)
(* ;; *)

(* let repo = Tests.commit repo *)
(* << *)
(*   infer ( *)
(*     letb ?X42 [add] *)
(*     letb (lam nat [x] lam nat [y] recb o y [z][_] add x z) [mul] *)
(*     app (app mul ?X43[add; infer add]) (s (infer^0 ?X23)) *)
(*   ) *)
(* >> *)
(* ;; *)

(* let repo = Tests.commit repo *)
(* << *)
(*   infer ( *)
(*     letb ?X42 [add] *)
(*     infer^0 ( *)
(*       inline ([x] ?X90[x]) ?X91 nat ?X92[add; infer add] *)
(*     ) *)
(*   ) *)
(* >> *)
(* ;; *)

42
