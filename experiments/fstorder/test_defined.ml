#use "load.ml"
;;

let repo = Slicer.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  two : nat = s (s o).

  plus : nat -> nat -> nat = $ fun m n ->
    let rec f = function
    | << o >> -> n
    | << s $m$ >> -> << s (plus $m$ $n$) >> (* or << s (plus $m$ $n$) >> *)
    | default -> f (Kernel.eval repo env default)
    in f m
  $.

  mult: nat -> nat -> nat = $ fun m n ->
    let rec f = function
      | << o >> -> << o >>
      | << s $m$ >> -> << plus $n$ (mult $m$ $n$) >>
      | default -> f (Kernel.eval repo env default)
    in f m
  $.

  exp : type.
  enat : nat -> exp.
  eplus : exp -> exp -> exp.
  emult : exp -> exp -> exp.

  eval : {e : exp} nat = $ fun e ->
    let rec f = function
      | << enat $n$ >> -> n
      | << eplus $e1$ $e2$ >> -> << plus (eval $e1$) (eval $e2$) >>
      | << emult $e1$ $e2$ >> -> << mult (eval $e1$) (eval $e2$) >>
      | default -> f (Kernel.eval repo env default)
    in f e
  $.
>>
;;

Tests.conv repo <<
  two
>> <<
  s (s o)
>> << nat >>
;;

Tests.conv repo <<
  plus o two
>> <<
  s (s o)
>> << nat >>
;;

Tests.commit repo <<
  plus two o
>>
;;

Tests.conv repo <<
  plus two o
>> <<
  s (s o)
>> << nat >>
;;

Tests.conv repo <<
  plus two two
>> <<
  s (s (s (s o)))
>>
;;

Tests.commit repo <<
  plus two two
>>
;;

Tests.commit_eq repo <<
  mult two two
>> <<
  plus two two
>>
;;

Tests.commit_eq repo <<
  plus (plus two two) (plus o (s o))
>> <<
  s (s (s (s (s o))))
>>
;;

Tests.commit_eq repo <<
  plus (s o) (mult two (s two))
>> <<
  s (s (s (s (s (s (s o))))))
>>
;;

Tests.commit_eq repo <<
  eval (eplus (enat (s o)) (emult (enat two) (enat (s two)))
)
>> <<
  plus (s o) (mult two (s two))
>>
;;

Tests.commit_eq repo <<
  eval (enat o)
>> <<
  o
>>
;;

Tests.commit_eq repo <<
  eval (eplus (enat o) (enat o))
>> <<
  o
>>
;;

Tests.commit_eq repo <<
  eval (eplus (enat two) (enat two))
>> <<
  s (s (s (s o)))
>>
;;

42
