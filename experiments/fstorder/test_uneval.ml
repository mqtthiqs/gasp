#use "load.ml"
;;

let repo = Version.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  pred : nat -> nat = $ fun x ->
    match* x with
      | << o >> -> << o >>
      | << s $n$ >> -> n
  $.

  f : nat -> nat = $ fun x ->
    match* x with
      | << pred $n$ >> -> << o >>
      | << o >> -> << o >>
      | << s $n$ >> -> << s $n$ >>
  $.

>>
;;

Tests.commit_eq repo <<
  pred (s (s o))
>> <<
  s o
>>
;;

Tests.commit_eq repo <<
  f (s o)
>> <<
  s o
>>
;;

Tests.commit_eq repo <<
  f (pred (s (s o)))
>> <<
  o
>>
;;

42
