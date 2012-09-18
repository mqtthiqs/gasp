#use "load.ml"
;;

let repo = Version.init
<:sign<

o : type.
P : {m : o -> o -> o} type.
p : {f : o -> o -> o} P [x] [y] f x y.

Q : {n : P [x] [y] x} type.

c : Q (p ([x][y] x)).

>>
;;
