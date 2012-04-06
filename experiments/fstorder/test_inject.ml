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

  omega : tm -> tm = $ fun m -> omega repo env m $.

  eta_exp : tm -> tm = $ fun m -> << lam [x] app $m$ x >> $.

  vars : tm -> tm = $ fun m ->
    match m rec Kernel.eval repo env with
      | << lam $m$ >> -> << lam [x] vars ($m$ x) >>
      | << app $m$ $n$ >> -> << app (vars $m$) (vars $n$) >>
      | << $id:x$ >> -> << $id:x$ >>
  $.

  inst : tm -> tm = $ fun m ->
    match m rec Kernel.eval repo env with
      | << lam $m$ >> -> << inst ($m$ (lam [x] x)) >>
      | << app $m$ $n$ >> -> << app $m$ $n$ >>
      | << $id:x$ >> -> << $id:x$ >>
  $.

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

Tests.commit_eq repo
<<
  lam [f] (eta_exp (app f f))
>> <<
  lam [f] (lam [x] app (app f f) x)
>>
;;

Tests.commit_eq repo
<<
  lam [f] (eta_exp (eta_exp f))
>> <<
  lam [f] (lam [x] app (lam [y] app f y) x)
>>
;;

Tests.commit_eq repo
<<
  lam [f] (eta_exp (eta_exp (app f f)))
>> <<
  lam [f] (lam [x] app (lam [y] app (app f f) y) x)
>>
;;

Tests.commit_eq repo
<<
  vars (lam [a] a)
>> <<
  lam [a] a
>>
;;

Tests.commit_eq repo
<< vars (lam [a] app a a) >>
<< lam [a] app a a >>
;;

Tests.commit_eq repo
<< vars (lam [x] lam [x] app x x) >>
<< lam [x] lam [x] app x x >>
;;

Tests.commit_eq repo
<< lam [a] (vars a) >>
<< lam [x] x >>
;;

Tests.commit_eq repo
<< lam [a] vars (lam [b] app a b) >>
<< lam [a] lam [b] app a b >>
;;

(* Fails here *)

Tests.commit_eq repo
<< vars (lam [a] lam [b] a) >>
<< lam [a] lam [b] a >>
;;

Tests.commit_eq repo
<< lam [x] vars (lam [b] app x b) >>
<< lam [x] lam [b] app x b >>
;;

Tests.commit_eq repo
<<
  vars (lam [a] lam [b] app a b)
>> <<
  lam [f] lam [x] app (s f) (s x)
>>
;;

(* TODO: tests inst *)

42
