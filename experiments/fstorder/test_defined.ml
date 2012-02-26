#use "load.ml"
;;

let test_commit repo m n =
  let repo = Slicer.commit repo m in
  let p = LF.Strat.obj repo.Repo.sign [] (Slicer.checkout repo) in
  let n = LF.Strat.obj repo.Repo.sign [] n in
  Kernel.Conv.obj repo (n, p);
  repo
;;

let repo = Slicer.init <:sign<
  nat : type.
  o : nat.
  s : nat -> nat.

  deux : nat = s (s o).

  plus : {m:nat} {n:nat} nat = $ match m with
    | << o >> -> n
    | << s $m$ >> -> << s $plus [m; n]$ >>
    | _ -> assert false
  $.

  exp : type.
  enat : nat -> exp.
  eplus : exp -> exp -> exp.

  exec : {e : exp} nat = $ match e with
    | << enat $n$ >> -> n
    | << eplus $e1$ $e2$ >> -> plus [exec [e1]; exec [e2]]
    | _ -> assert false
  $.
>>
;;

test_commit repo <<
  exec (eplus (enat (s (s o))) (enat (s (s o))))
>> <<
  s (s (s (s o)))
>>
;;

42
