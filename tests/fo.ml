#use "load.ml"
;;

(* First-order STLC *)
let stlc = Version.init
<:sign<
  tp : type.
  base : tp.
  arr : tp -> tp -> tp.

  tm : type.
  lam : tm -> tm.
  #app : tm -> tm -> tm.
  s : tm -> tm.
  z : tm.

  env : type.
  nil : env.
  cons : tp -> env -> env.

  is : env -> tm -> tp -> type.

  is_weak : {E:env} {M:tm} {A:tp} {B:tp}
    is E M B -> is (cons A E) (s M) B.
  is_var : {E:env} {M:tm} {A:tp}
    is (cons A E) z A.
  is_app : {E:env} {M:tm} {N:tm} {A:tp} {B:tp}
    is E M (arr A B) -> is E N A -> is E (app M N) B.
  is_lam : {E:env} {M:tm} {A:tp} {B:tp}
    is (cons B E) M A -> is E (lam M) A.
>>
;;

let repo = Version.commit stlc []
  <<
    lam (lam (app (s z) z))
  >>
;;
