#use "load.ml"
;;

open Util

let repo = Version.init
<:sign<
  A : type.
  B : type.
  C : type.
  a : A.
  b : B.
  c : C.
  P : A -> type.
  Q : {x : A} P x -> type.
>>
;;

Tests.inv repo 0 <<
  A -> B -> C
>> <<
  A -> B -> C -> A
>>
;;

Tests.inv repo 1 <<
  A -> B -> C
>> <<
  A -> B -> C -> B
>>
;;

Tests.inv repo 0 <<
  {x : A} {p : P x} Q x p
>> <<
  {x : A} {p : P x} Q x p -> A
>>
;;

Tests.inv repo 1 <<
  {x : A} {p : P x} Q x p
>> <<
  {x : A} {p : P x} Q x p -> P x
>>
;;

42
