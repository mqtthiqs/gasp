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

  T : type.
  lam : (T -> T) -> T.
  id : T -> T = $ fun x -> return x $.
  f : T -> T = $ fun t -> match* t with
    | << lam $t$ >> ->
        match* << lam [x] f ($t$ (f^0 x x)) >> with
          | t -> return t
  $.
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

(* This works without problems *)
Tests.commit_eq repo <<
  f (lam [x] x)
>> <<
  lam [x] x
>>
;;

(* But if we insert an id on the way, it hides the reduction of f (f^0 x x) -> x: *)
(* Steps:
   - f (lam [x] id x)
   - lam [x] f (id (f^0 x x))
   - lam [x] f x
 *)
Tests.commit_eq repo <<
  f (lam [x] id x)
>> <<
  lam [x] x
>>
;;

42
