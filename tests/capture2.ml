#use "load.ml"
;;

let repo = Version.init
<:sign<

o : type.
P : ((o -> o) -> o -> o) -> type.
p : {F:(o -> o) -> o -> o} P ([f] [x] F ([y] f y) x).

Q : P ([x] [z] x z) -> type.
c : Q (p ([y] [x] y x)).

>>
;;
