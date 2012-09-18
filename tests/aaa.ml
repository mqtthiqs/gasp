#use "load.ml"
;;

let repo = Version.init
<:sign<
  A : type.
  a : A.
  P : A -> A -> type.
  f : A -> A.
  a : A.
  p : P (f a) (f a).
>>
;;
