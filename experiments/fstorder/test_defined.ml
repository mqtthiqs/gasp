#use "load.ml"
;;

let reduce repo env m =
  Format.printf "**** reduce: %a@." SLF.Printer.term m;
  let env = SLF.Strat.env repo.Struct.Repo.sign env in
  let repo = Slicer.commit repo env m in
  Slicer.checkout repo

let repo = Slicer.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.

  two : nat = s (s o).

  plus : {m:nat} {n:nat} nat = $ fun m n -> match m with
    | << o >> -> n
    | << s $m$ >> -> << s (plus $m$ $n$) >>
    | << $m$ >> -> << plus $reduce repo <:env< >> m$ $n$ >>
  $.

  exp : type.
  enat : nat -> exp.
  eplus : exp -> exp -> exp.

  eval : {e : exp} nat = $ fun e -> match e with
    | << enat $n$ >> -> n
    | << eplus $e1$ $e2$ >> -> << plus (eval $e1$) (eval $e2$) >>
    | << $e$ >> -> reduce repo <:env< >> << eval $e$ >>
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
