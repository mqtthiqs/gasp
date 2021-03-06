#use "load.ml"
;;

let repo = Version.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  two : nat = $return << s (s o) >>$.

  plus : nat -> nat -> nat = $ fun m n ->
    match* m with
      | << o >> -> return n
      | << s $m$ >> -> return << s (plus $m$ $n$) >>
  $.

  mult: nat -> nat -> nat = $ fun m n ->
    match* m with
      | << o >> -> return << o >>
      | << s $m$ >> -> return << plus $n$ (mult $m$ $n$) >>
  $.

  div2 : nat -> nat = $ fun m ->
    match* m with
      | << o >> -> return << o >>
      | << s $m$ >> -> match* m with
          | << o >> -> failwith "div2"
          | << s $m$ >> -> return << div2 $m$ >>
  $.

  exp : type.
  enat : nat -> exp.
  eplus : exp -> exp -> exp.
  emult : exp -> exp -> exp.

  eval : {e : exp} nat = $ fun e ->
    match* e with
      | << enat $n$ >> -> return n
      | << eplus $e1$ $e2$ >> -> return << plus (eval $e1$) (eval $e2$) >>
      | << emult $e1$ $e2$ >> -> return << mult (eval $e1$) (eval $e2$) >>
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
>> << nat >>
;;

Tests.commit_eq repo <<
  plus two two
>> <<
  s (s (s (s o)))
>>
;;

Tests.commit_eq repo <<
  mult two two
>> <<
  s (s (s (s o)))
>>
;;

Tests.commit_eq repo <<
  plus o (plus two two)
>> <<
  s (s (s (s o)))
>>
;;

Tests.commit_eq repo <<
  plus (plus (s o) o) o
>> <<
  s o
>>
;;

Tests.commit_eq repo <<
  div2 (s (s o))
>> << o >>
;;

Tests.commit_eq repo <<
  plus (plus o (s o)) o
>> <<
  s o
>>
;;

Tests.commit_eq repo <<
  plus (plus two two) o
>> <<
  s (s (s (s o)))
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
  s (s (s (s (s (s (s o))))))
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
