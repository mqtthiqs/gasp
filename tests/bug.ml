#use "load.ml"
;;

let repo = Version.init
<:sign<
nat : type.
o : nat.

vec : nat -> type.

fill : {n : nat} vec n.

empty : vec o -> type.

all_i : empty (fill o).
>>
;;
