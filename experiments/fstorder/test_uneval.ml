#use "load.ml"
;;

let repo = Slicer.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  pred : nat -> nat = $ fun x ->
    match x rec eval <:env< >> with
      | << o >> -> << o >>
      | << s $n$ >> -> n
  $.

  f : nat -> nat = $ fun x ->
    match x rec eval <:env< >> with
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
