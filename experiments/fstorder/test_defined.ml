#use "load.ml"
;;

let test_commit repo m n =
  let repo = Slicer.commit repo m in
  let p = SLF.Strat.obj repo.Struct.Repo.sign [] (Slicer.checkout repo) in
  let n = SLF.Strat.obj repo.Struct.Repo.sign [] n in
  Kernel.Conv.obj repo (n, p);
  repo
;;

let repo = Slicer.init <:sign<
  nat : type.
  o : nat.
  s : nat -> nat.

  two : nat = s (s o).

  plus : {m:nat} {n:nat} nat = $ match m with
    | << o >> -> n
    | << s $m$ >> -> << s (plus $m$ $n$) >>
    | _ -> assert false
  $.

  exp : type.
  enat : nat -> exp.
  eplus : exp -> exp -> exp.

  eval : {e : exp} nat = $ match e with
    | << enat $n$ >> -> n
    | << eplus $e1$ $e2$ >> -> << plus (eval $e1$) (eval $e2$) >>
    | << ?X[$s$] >> -> failwith "meta"
    | << $a$ >> -> SLF.Printer.term Format.std_formatter a; failwith ("?")
  $.
>>
;;

test_commit repo <<
  s (eval (eplus (enat two) (enat two)))
>> <<
  s (s (s (s (s o))))
>>
;;

42
