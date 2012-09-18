#use "load.ml"
;;

let repo = Version.init
<:sign<
A : type.
B : type.
C : type.
f : A -> B.
P : B -> type.
c : {x:A} P (f x).

a : A.

Q : P (f a) -> type.

q : Q (c a).
>>
;;
