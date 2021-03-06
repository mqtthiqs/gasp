#use "load.ml"
;;

let repo = Version.init
<:sign<
A : type.
B : type.
a : A.
b : A.
g : A -> B.

P : B -> type.
R : P (g a) -> P (g b) -> type.
f : {x : A} P (g x).

r : {H1 : P (g a)} {H2 : P (g b)} R H1 H2.

commit : {x : P (g a)} {y : (P (g b))} R x y -> Commit.
>>
;;

let repo = Tests.commit repo <<
  commit (f a) (f b) (r (f a) (f b))
>>
;;
