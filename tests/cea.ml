#use "load.ml"
;;

let repo = Version.init
<:sign<

A : type.
B : type.
C : type.
D : type.
a : (D -> B) -> C -> A.
b : C -> B.
b' : C -> B.
c : D -> C.
d : D.

commit : A -> Commit.

>>
;;

let repo = Tests.commit repo <<
commit (
  a ([x] b (c x)) (c d)
)
>>
;;
