#use "load.ml"
;;

let repo = Slicer.init
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

let repo = Slicer.commit repo Struct.Env.empty
<<
  p a b
>>
;;

let repo = Slicer.commit repo Struct.Env.empty
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

(* TODO: comprendre l'assert false ici *)
(* Tests.subst repo *)
(* << [x] [y] f x y a >> *)
(* << [X] X a b a >> *)
(* << f a b a >> *)
(* << B >> *)
(* ;; *)

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
<< [a] ?X4[a] >>
<< [X] [x] X x >>
<< [x] ?X4[x] >>
<< A -> A >>
;;

