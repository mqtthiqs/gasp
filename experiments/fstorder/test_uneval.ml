#use "load.ml"
;;

let repo = Slicer.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  pred : nat -> nat = $ fun x ->
    let rec f = function
      | << o >> -> << o >>
      | << s $n$ >> -> n
      | default -> f (Kernel.eval repo env default)
    in f x
  $.

  f : nat -> nat = $ fun x ->
    let rec f = function
      | << pred $n$ >> -> << o >>
      | << o >> -> << o >>
      | << s $n$ >> -> << s $n$ >>
      | default -> f (Kernel.eval repo env default)
    in f x
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
