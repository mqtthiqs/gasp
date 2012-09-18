#use "load.ml"
;;

(* HOAS STLC *)
let repo = Version.init
<:sign<
  tp : type.
  #base : tp.
  arr : tp -> tp -> tp.
  tm : type.
  lam : (tm -> tm) -> tm.
  app : tm -> tm -> tm.
  is : tm -> tp -> type.
  is_app : {M:tm} {N:tm} {A:tp} {B:tp}
    is M (arr A B) -> is N A -> is (app M N) B.
  is_lam : {M:tm -> tm} {A:tp} {B:tp}
    ({t : tm} is t B -> is (M t) A) -> is (lam [u] M u) (arr A B).
  sorry : {M : tm} {A: tp} is M A.
>>
;;

(* a term *)
Tests.commit repo
<<
  lam [x] lam [y] app (app x y) y
>>
;;

(* some derivations *)
Tests.commit repo
<<
  is_lam ([x] x) base base ([_] [H] H)
>>
;;

Tests.commit repo
<<
  is_lam ([x] app x x) base base
  [x] [_] sorry (app x x) base
>>
;;

Tests.commit repo
<<
  is_lam ([_] lam [x] x) (arr base base) base
  [_] [H1] is_lam ([y] y) base base ([_] [H2] H2)
>>
;;

Tests.commit repo
<<
  is_lam ([_] lam [x] x) (arr base base) base
  [_] [H1] is_lam ([y] y) base base ([_] [H2] H2)
>>
;;

42
