#use "load.ml"
;;

let repo = Version.init
<:sign<
  A : type.
  B : type.
  P : B -> type.
  #a : A.
  #b : A.
  f : A -> A -> A -> B.
  g : A -> A.
  h : (A -> A) -> A.
  p : {x : A} {y : A} P (f x y x).
  Q : P (f a b b) -> type.
>>
;;

let repo = Version.commit repo []
<<
  p a b
>>
;;

let repo = Version.commit repo []
<<
  h [x] g x
>>
;;

Tests.subst repo
<< a >>
<< [X] X >>
<< a >>
<< A >>
;;

Tests.subst repo
<< a >>
<< [X] [y] f X y y >>
<< [y] f a y y >>
<< A -> B >>
;;

Tests.subst repo
<< [x] [y] x >>
<< [X] X a b >>
<< a >>
<< A >>
;;

Tests.subst repo
<< [x] [y] [z] f x y z >>
<< [X] X a b a >>
<< f a b a >>
<< B >>
;;

Tests.subst repo
<< [x] x >>
<< [X] [y] X y>>
<< [x] x >>
<< A -> A >>
;;

Tests.subst repo
<< [x] [y] x >>
<< [X] [z] X z a >>
<< [z] z >>
<< A -> A >>
;;

Tests.subst repo
<< [a] ?X3[a] >>
<< [X] [x] X x >>
<< [x] ?X3[x] >>
<< A -> A >>
;;

try Tests.fail5
Tests.subst' repo
<< [x] x x >>
<< [y] y y >>
<< [x] x >>
<< A -> A >>
with Tests.Failed (LF.Not_eta _) -> ()
;;

Tests.subst_open
(Version.init  <:sign< A : type. B : type. C : type. >>)
<:env< g : A -> B -> C >>
<< [a] [b] g a b >>
<< [X] [x] [y] X x y >>
<< [a] [b] g a b >>
<< A -> B -> C >>
;;
