#use "load.ml"
;;

let repo = Version.init
<:sign<

nat : type.
o : nat.
P : nat -> type.
po : P o.
Q : {n : nat} P n -> type.
q : Q o po.

>>
;;
