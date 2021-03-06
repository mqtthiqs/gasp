#use "load.ml"
;;

open Util

let repo = Version.init
<:sign<

  unit : type.
  one : unit.

  tp : type.
  #base : tp.
  arr : tp -> tp -> tp.

  tm : type.
  lam : tp -> (tm -> tm) -> tm.
  app : tm -> tm -> tm.

  is : tm -> tp -> type.
  is_app : {M:tm} {N:tm} {A:tp} {B:tp}
    is M (arr A B) -> is N A -> is (app M N) B.
  is_lam : {M:tm -> tm} {A:tp} {B:tp}
    ({t : tm} is t A -> is (M t) B) -> is (lam A [u] M u) (arr A B).

  inf : tm -> type.
  ex : {M : tm} {A : tp} {H : is M A} inf M.

  equals : tp -> tp -> unit = $ fun a b ->
    match* a with
      | << base >> ->
          begin match* b with
            | << base >> -> return << one >>
            | << arr $_$ $_$ >> -> failwith "types not equal"
          end
      | << arr $a1$ $a2$ >> ->
          begin match* b with
            | << arr $b1$ $b2$ >> ->
                let* << one >> = << equals $a1$ $a2$ >> in
                return << equals $b1$ $b2$ >>
            | << base >> -> failwith "types not equal"
          end
    $.

  infer : {M : tm} inf M = $ fun m ->
    Debug.log_open "infer" "%a" SLF.Printer.term m;
    let repo, r = match* m with
      | << lam $a$ $m$ >> ->
          let* << ex $_$ $b$ $d$ >> in <:env< x:tm; h:is x $a$ >> =
            << infer ($m$ (infer^0 x (ex x $a$ h))) >> in
          return << ex (lam $a$ $m$) (arr $a$ $b$) (is_lam $m$ $a$ $b$ ([x] [h] $d$)) >>
      | << app $m$ $n$ >> ->
          let* << ex $_$ $c$ $d1$ >> = << infer $m$ >> in
          let* << ex $_$ $a'$ $d2$ >> = << infer $n$ >> in
          begin match* c in <:env< >> with
            | << arr $a$ $b$ >> ->
                let* << one >> = << equals $a$ $a'$ >> in
                return << ex (app $m$ $n$) $b$ (is_app $m$ $n$ $a$ $b$ $d1$ $d2$) >>
            | << $id:_$ >> -> failwith "non-functional application"
          end
    in
    Debug.log_close "infer" "=> %a in %a" SLF.Printer.term r SLF.Printer.repo_light repo;
    repo, r
  $.


  prj : {M : tm} {A : tp} inf M -> is M A = $ fun _ _ i ->
    match* i with
      | << ex $_$ $_$ $h$ >> -> return h
  $.

>>
;;

Tests.commit repo
<<
  infer (lam base [z] z)
>>
;;

Tests.commit repo
<<
  infer (lam (arr base base) [x] lam base [y] app x y)
>>
;;

Tests.commit repo
<<
  is_lam ([t] t) base base ([x] [h] prj x base (infer (infer^0 x (ex x base h))))
>>
;;

(* This should be no work at all *)
Tests.commit repo
<<
  infer (infer^0 (lam base [z] z)
           (ex (lam base [y] y) (arr base base)
              (is_lam ([t] t) base base ([_] [H] H))))
>>
;;

42
