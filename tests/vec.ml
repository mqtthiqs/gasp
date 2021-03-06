#use "load.ml"
;;

let repo = Version.init
<:sign<

nat : type.
o : nat.
s : {n : nat} nat.

A : type.
a : A.

vec : {l : nat} type.
nil : vec o.
cons : {len : nat} {hd : A} {tl : vec len} vec (s len).

P : {n : nat} vec n -> type.

(* % p : P (s (s o)) (cons (s (s o)) o (cons (s o) o (cons o nil))). *)

>>
