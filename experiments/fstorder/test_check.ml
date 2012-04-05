#use "load.ml"
;;

open Util

let repo = Slicer.init
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

  get : {M : tm} inf M -> tm = $ fun m _ -> m $.

  equals : tp -> tp -> unit = $ fun a b ->
    match a rec Kernel.eval repo env with
      | << base >> ->
          begin match b rec Kernel.eval repo env with
            | << base >> -> << unit >>
            | << arr $_$ $_$ >> -> failwith "types not equal"
          end
      | << arr $a1$ $a2$ >> ->
          begin match b rec Kernel.eval repo env with
            | << arr $b1$ $b2$ >> -> ignore (equals repo env a1 a2); equals repo env b1 b2
            | << base >> -> failwith "types not equal"
          end
    $.

  infer : {M : tm} inf M = $ fun m ->
    Debug.log_open "infer" "%a" SLF.Printer.term m;
    let r = match m rec Kernel.eval repo env with
      | << lam $a$ $m$ >> ->
          let env = SLF.Strat.env repo.Struct.Repo.sign env <:env< x:tm; h:is x $a$ >> in
          begin match infer repo env << $m$ (get x (ex x $a$ h)) >> rec Kernel.eval repo env with
            | << ex $_$ $b$ $d$ >> ->
                << ex (lam $a$ $m$) (arr $a$ $b$) (is_lam $m$ $a$ $b$ ([x] [h] $d$)) >>
          end
      | << app $m$ $n$ >> ->
          begin match infer repo env m rec Kernel.eval repo env with
            | << ex $_$ $c$ $d1$ >> ->
                match c rec Kernel.eval repo env with
                  | << arr $a$ $b$ >> ->
                      match infer repo env n rec Kernel.eval repo env with
                        | << ex $_$ $a'$ $d2$ >> ->
                            ignore (equals repo env a a');
                            << ex (app $m$ $n$) $b$ (is_app $m$ $n$ $a$ $b$ $d1$ $d2$) >>
          end
      | << get $x$ $i$ >> -> i
    in
    Debug.log_close "infer" "=> %a" SLF.Printer.term r;
    r
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

(* This should be no work at all *)
Tests.commit repo
<<
  infer (get (lam base [z] z)
           (ex (lam base [y] y) (arr base base)
              (is_lam ([t] t) base base ([_] [H] H))))
>>
;;

42
