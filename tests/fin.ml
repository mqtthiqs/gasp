#use "load.ml"
;;

let repo = Version.init
<:sign<

nat : type.
o : nat.
s : {p : nat} nat.

fin : {l : nat} type.
if1 : {n : nat} fin (s n).
ifs : {n : nat} fin n -> fin (s n).

>>
;;
