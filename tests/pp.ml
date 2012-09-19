#use "load.ml"
;;

let repo = Version.init
<:sign<

nat : type.
o : nat.
s : nat -> nat.
P : nat -> type.
P' : nat -> nat -> type.
Q : {x : nat} P x -> P x.
R : {x : nat} P x -> P (s o).
S : {x : nat} {y : nat} P' x y.

>>
;;
