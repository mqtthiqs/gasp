#use "load.ml"
;;

let repo = Version.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  f : nat -> nat = $ fun x ->
    match* x with
      | << o >> -> return << o >>
      | << s $n$ >> -> return << s $n$ >>
  $.

>>
;;

Tests.commit_eq repo <<
  f (s (s o))
>> <<
  s (s o)
>>
;;

Tests.commit_eq repo <<
  f (s o)
>> <<
  s o
>>
;;

Tests.commit_eq repo <<
  f (s (s o))
>> <<
  s (s o)
>>
;;

42
