#use "load.ml"
;;

open Util

let repo = Version.init
<:sign<

  tm : type.
  lam : (tm -> tm) -> tm.
  app : tm -> tm -> tm.

  subst : (tm -> tm) -> tm -> tm = $ fun m n -> return << $m$ $n$ >> $.

  subst2 : tm -> tm -> tm = $ fun m n ->
    match* m with
      | << lam $m$ >> -> return << $m$ $n$ >>
  $.

  omega : tm -> tm = $ fun m ->
    match* << omega $m$ >> with
      | << lam $m$ >> -> return << lam $m$ >>
      | << app $m$ $n$ >> -> return << app $m$ $n$ >>
      | << $id:x$ >> -> return << $id:x$ >>
  $.

  eta_exp : tm -> tm = $ fun m -> return << lam [x] app $m$ x >> $.

  vars : tm -> tm = $ fun m ->
    match* m with
      | << lam $m$ >> -> return << lam [x] vars ($m$ x) >>
      | << app $m$ $n$ >> -> return << app (vars $m$) (vars $n$) >>
      | << $id:x$ >> -> return << $id:x$ >>
  $.

  inst : tm -> tm = $ fun m ->
    match* m with
      | << lam $m$ >> -> return << inst ($m$ (lam [x] x)) >>
      | << app $m$ $n$ >> -> return << app $m$ $n$ >>
      | << $id:x$ >> -> return << $id:x$ >>
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
<< vars (lam [a] lam [b] a) >>
<< lam [a] lam [b] a >>
;;

Tests.commit_eq repo
<< lam [a] vars (lam [b] app a b) >>
<< lam [a] lam [b] app a b >>
;;

Tests.commit_eq repo
<< lam [x] vars (lam [b] app x b) >>
<< lam [x] lam [b] app x b >>
;;

Tests.commit_eq repo
<<
  vars (lam [a] lam [b] app a b)
>> <<
  lam [f] lam [x] app f x
>>
;;

42
