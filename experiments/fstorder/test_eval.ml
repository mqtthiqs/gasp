#use "load.ml"
;;

open Util

let repo = Version.init <:sign<
  nat : type.
  #o : nat.
  s : nat -> nat.
  p : nat -> nat.

  norm : nat -> nat = $ fun x ->
    match* x with
      | << o >> -> << o >>
      | << s $n$ >> -> << s (norm $n$) >>
      | << p $n$ >> -> match* << norm $n$ >> with
          | << o >> -> << o >>
          | << s $n$ >> -> << $n$ >>
  $.
>>
;;

Tests.commit_eq repo <<
  norm (s (p (s o)))
>> <<
  s o
>>
;;

let repo = Version.init <:sign<

  tm : type.
  lam : (tm -> tm) -> tm.
  app : tm -> tm -> tm.

  rename : tm -> tm = $ fun n ->
    Debug.log_open "rename" "%a" SLF.Printer.term n;
    let r = match* n with
      | << $id:x$ >> -> << $id:x$ >>
      | << app $m$ $n$ >> -> << app (rename $m$) (rename $n$) >>
      | << lam $n$ >> -> << lam [x] rename ($n$ x) >>
    in
    Debug.log_close "rename" "=> %a" SLF.Printer.term r;
    r
  $.

>>
;;

Tests.commit_eq repo <<
  rename (lam [a] a)
>> <<
  lam [x] x
>>
;;

Tests.commit_eq repo <<
  rename (lam [x] x)
>> <<
  lam [x] x
>>
;;

Tests.commit_eq repo <<
  rename (lam [x] lam [y] x)
>> <<
  lam [x] lam [y] x
>>
;;

Tests.commit_eq repo <<
  rename (lam [x] lam [y] y)
>> <<
  lam [x] lam [y] y
>>
;;

Tests.commit_eq repo <<
  lam [x] rename (lam [y] lam [z] x)
>> <<
  lam [x] lam [y] lam [z] x
>>
;;

Tests.commit_eq repo <<
  lam [x] rename x
>> <<
  lam [x] x
>>
;;

Tests.commit_eq repo <<
  lam [a] rename a
>> <<
  lam [z] z
>>
;;

42

(* TODO que faire de ça?!?! *)
(*
let repo = Version.init <:sign<

  A : type.
  a : A.
  plus : A -> A -> A.

  f : A -> A = $ fun x ->
    match* x with
      | << plus $x$ $y$ >> -> << $x$ >>
      | << a >> -> << a >>
  $.

  P : A -> type.
  p : {x:A} P (f x).

>>
*)
