#use "load.ml"
;;

(* Test T: batch tests *)

open Test_check_T
;;

(* #load "test_check_T.cmo";; *)

(* not incredibly useful... *)
(* Tests.commit repo *)
(* << *)
(*   infer (lam nat [x] infer^0 x (infer x)) *)
(* >> *)
(* ;; *)


Tests.commit repo
<<
  infer (letb o [x] x)
>>
;;

Tests.commit repo
<<
  infer (lam nat [z] z)
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
<< infer (recb o o [x] [_] s x) >>
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
    letb (lam nat [x] lam nat [y] recb x y [z] [_] s z) [add]
    letb (lam nat [x] lam nat [y] recb o y [z] [_] app (app add x) z) [mult]
    letb (lam nat [x] lam nat [y] recb (s o) y [z] [_] app (app mult x) z) [exp]
    letb (lam nat [x] recb o x [_] [w] w) [pred]
    app (app exp (s o)) (s o)
  )
>>
;;
