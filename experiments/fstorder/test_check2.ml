#use "load.ml"
;;

open Util

let repo = Version.init
<:sign<

  unit : type.
  one : unit.

  tp : type.
  #bool : tp.
  #nat : tp.
  arr : tp -> tp -> tp.

  tm : type.
  lam : tp -> (tm -> tm) -> tm.
  app : tm -> tm -> tm.
  o : tm.
  s : tm -> tm.
  tt : tm.
  ff : tm.
  letb : tm -> (tm -> tm) -> tm.
  ifb : tm -> tm -> tm -> tm.

  is : tm -> tp -> type.
  is_app : {M:tm} {N:tm} {A:tp} {B:tp}
    is M (arr A B) -> is N A -> is (app M N) B.
  is_lam : {M:tm -> tm} {A:tp} {B:tp}
    ({t : tm} is t A -> is (M t) B) -> is (lam A [u] M u) (arr A B).
  is_o : is o nat.
  is_s : {M:tm} is M nat -> is (s M) nat.
  is_tt : is tt bool.
  is_ff : is ff bool.
  is_if : {M : tm} {N1 : tm} {N2 : tm} {A : tp}
    is M bool -> is N1 A -> is N2 A -> is (ifb M N1 N2) A.
  is_let : {M : tm} {N : tm -> tm} {A : tp} {B : tp}
    is M A -> ({x : tm} is x A -> is (N x) B) -> is (letb M [x] N x) B.

  inf : tm -> type.
  ex : {M : tm} {A : tp} {H : is M A} inf M.

  get : {M : tm} inf M -> tm = $ fun m _ -> m $.

  equals : tp -> tp -> unit = $ fun a b ->
    match a rec eval <:env< >> with
      | << $id:x$ >> ->
          begin match b rec eval <:env< >> with
            | << $id:y$ >> when x=y -> << one >>
            | << arr $_$ $_$ >> -> failwith "types not equal"
          end
      | << arr $a1$ $a2$ >> ->
          begin match b rec eval <:env< >> with
            | << arr $b1$ $b2$ >> ->
                begin match << equals $a1$ $a2$ >> rec eval <:env< >> with
                  | << one >> -> match << equals $b1$ $b2$ >> rec eval <:env< >> with
                      | << one >> -> << one >>
                end
            | << $id:x$ >> -> failwith "types not equal"
          end
    $.

  infer : {M : tm} inf M = $ fun m ->
    Debug.log_open "infer" "%a" SLF.Printer.term m;
    let r = match m rec eval <:env< >> with
      | << lam $a$ $m$ >> ->
          begin match << infer ($m$ (get x (ex x $a$ h))) >>
          rec eval <:env< x:tm; h:is x $a$ >> with
            | << ex $_$ $b$ $d$ >> ->
                << ex (lam $a$ $m$) (arr $a$ $b$) (is_lam $m$ $a$ $b$ ([x] [h] $d$)) >>
          end
      | << app $m$ $n$ >> ->
          begin match << infer $m$ >> rec eval <:env< >> with
            | << ex $_$ $c$ $d1$ >> ->
                match c rec eval <:env< >> with
                  | << arr $a$ $b$ >> ->
                      match << infer $n$ >> rec eval <:env< >> with
                        | << ex $_$ $a'$ $d2$ >> ->
                            match << equals $a$ $a'$ >> rec eval <:env< >> with
                              | << one >> ->
                                  << ex (app $m$ $n$) $b$ (is_app $m$ $n$ $a$ $b$ $d1$ $d2$) >>
          end
      | << o >> -> << ex o nat is_o >>
      | << s $m$ >> ->
          begin match << infer $m$ >> rec eval <:env< >> with
            | << ex $_$ nat $d$ >> -> << ex (s $m$) nat (is_s $m$ $d$) >>
          end
      | << tt >> -> << ex tt bool is_tt >>
      | << ff >> -> << ex ff bool is_ff >>
      | << ifb $m$ $n$ $p$ >> ->
          begin match << infer $m$ >> rec eval <:env< >> with
            | << ex $_$ bool $d$ >> ->
                begin match << infer $n$ >> rec eval <:env< >> with
                  | << ex $_$ $a$ $d1$ >> ->
                      begin match << infer $p$ >> rec eval <:env< >> with
                        | << ex $_$ $a'$ $d2$ >> ->
                            begin match << equals $a$ $a'$ >> rec eval <:env< >> with
                              | << one >> ->
                                  << ex (ifb $m$ $n$ $p$) $a$ (is_if $m$ $n$ $p$ $a$ $d$ $d1$ $d2$) >>
                            end
                      end
                end
          end
      | << letb $m$ $n$ >> ->
          begin match << infer $m$ >> rec eval <:env< >> with
            | << ex $_$ $a$ $d1$ >> ->
                begin match << infer ($n$ (get x (ex x $a$ h))) >> rec eval <:env< x:tm; h:is x $a$ >> with
                  | << ex $_$ $b$ $d2$ >> ->
                      << ex (letb $m$ $n$) $b$ (is_let $m$ $n$ $a$ $b$ $d1$ ([x] [h] $d2$)) >>
                end
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
  infer (letb o [x] x)
>>
;;

Tests.commit repo
<<
  infer (lam nat [z] z)
>>
;;

Tests.commit repo
<<
  infer (lam (arr nat nat) [x] lam nat [y] app x y)
>>
;;

Tests.commit repo
<<
  infer (lam bool [b] ifb b (s o) o)
>>
;;

Tests.commit repo
<<
  infer (
    letb (lam bool [b] ifb b (s o) o) [f]
    letb (app f tt) [x]
    letb (app f ff) [y]
    x
  )
>>
;;

42
