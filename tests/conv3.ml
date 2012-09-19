#use "load.ml"
;;

let repo = Version.init
<:sign<

A : type.
a : A -> A.

P : (A -> A) -> type.
Q : P [x] a x -> type.

p : {f : A -> A} P [x] f x.

c : Q (p [x] a x).
d : Q (p [x] a x).

>>
;;
