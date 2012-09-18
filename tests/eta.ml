#use "load.ml"
;;

let repo = Version.init
<:sign<

A : type.
B : type.
s : A -> B -> A.
X : (A -> B -> A) -> type.
P : X [x] [y] s x y.

>>
;;
