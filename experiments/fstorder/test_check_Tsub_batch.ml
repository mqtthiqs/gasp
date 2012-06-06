#use "load.ml"
;;

open Test_check_Tsub

(* #load "test_check_Tsub.cmo" *)
(* ;; *)

(* Test Tsub: batch tests *)

Tests.commit repo
<<
  infer (lam nat [z] z)
>>
;;

Tests.commit repo
<<
  infer (letb o [x] x)
>>
;;

Tests.commit repo
<<
  infer (lam (arr nat nat) [x] lam nat [y] app x y)
>>
;;

Tests.commit repo
<<
  infer (lam bool [b] ifb b (s o) o)
>>
;;

Tests.commit repo
<<
  infer (
    letb (lam bool [b] ifb b (s o) o) [f]
    letb (app f tt) [x]
    letb (app f ff) [y]
    x
  )
>>
;;

Tests.commit repo
<< infer (recb o o [bla] [_] s bla) >>
;;

Tests.commit repo
<<
  infer (
    lam nat [x] lam nat [y] recb x y [z] [_] s z
  )
>>
;;

Tests.commit repo
<<
  infer (
    lam (arr nat nat) [div2]
      recb o (s o) [_] [y] app div2 y
  )
>>
;;

(* Contrex de Pierre B. *)
try Tests.fail2
  Tests.commit repo
<<
  infer (
    lam (arr even nat) [div2]
      recb (s (s o)) (s (s o)) [_] [y] app div2 y
  )
>>
with Tests.Failed (Failure "subtype error") -> ()
;;

Tests.commit repo
<<
  infer (
    letb (lam nat [x] lam nat [y] recb x y [z] [_] s z) [add]
    letb (lam nat [x] lam nat [y] recb o y [z] [_] app (app add x) z) [mult]
    letb (lam nat [x] lam nat [y] recb (s o) y [z] [_] app (app mult x) z) [exp]
    letb (lam nat [x] recb o x [_] [w] w) [pred]
    app (app exp (s o)) (s o)
  )
>>
;;

(* invalid test (TODO understand) *)
(* Tests.commit repo *)
(* << *)
(*   infer (lam nat [x] infer^0 x (infer x)) *)
(* >> *)
(* ;; *)
