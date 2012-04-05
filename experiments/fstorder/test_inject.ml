#use "load.ml"
;;

open Util

let repo = Slicer.init
<:sign<

  tm : type.
  lam : (tm -> tm) -> tm.
  app : tm -> tm -> tm.

  subst : (tm -> tm) -> tm -> tm = $ fun m n -> << $m$ $n$ >> $.

  subst2 : tm -> tm -> tm = $ fun m n ->
    match m rec Kernel.eval repo env with
      | << lam $m$ >> -> << $m$ $n$ >>
      $.

  eta_exp : tm -> tm = $ fun m -> << lam [x] app $m$ x >> $.

>>
;;

Tests.commit_eq repo
<<
  subst ([x] x) (lam [z] app z z)
>> <<
  lam [x] app x x
>>
;;

Tests.commit_eq repo
<<
  subst ([x] x) (subst ([z] app z z) (lam [y] y))
>> <<
  app (lam [x] x) (lam [x] x)
>>
;;

Tests.commit_eq repo
<<
  subst2 (lam [x] x) (lam [z] app z z)
>> <<
  lam [x] app x x
>>
;;

Tests.commit_eq repo
<<
  subst2 (lam [x] x) (subst2 (lam [z] app z z) (lam [y] y))
>> <<
  app (lam [x] x) (lam [x] x)
>>
;;

Tests.commit_eq repo
<<
  subst2 (subst2 (lam [y] y) (lam [z] app z z)) (lam [x] x)
>> <<
  app (lam [x] x) (lam [x] x)
>>
;;

Tests.commit_eq repo
<<
  lam [f] (eta_exp f)
>> <<
  lam [f] (lam [x] app f x)
>>
;;

42








