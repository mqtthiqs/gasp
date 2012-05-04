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
  recb : tm -> tm -> (tm -> tm -> tm) -> tm.

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
  is_rec : {M : tm} {N : tm} {P : tm -> tm -> tm} {A : tp}
    is M nat ->
    is N A ->
    ({x:tm} {y:tm} is x nat -> is y A -> is (P x y) A) ->
    is (recb M N [x] [y] P x y) A
   .

  inf : tm -> type.
  ex : {M : tm} {A : tp} {H : is M A} inf M.

  get : {M : tm} inf M -> tm = $ fun m _ -> m $.

  equals : tp -> tp -> unit = $ fun a b ->
    match* a with
      | << $id:x$ >> ->
          begin match* b with
            | << $id:y$ >> when x=y -> << one >> (* TODO: equality *)
            | << arr $_$ $_$ >> -> failwith "types not equal"
          end
      | << arr $a1$ $a2$ >> ->
          begin match* b with
            | << arr $b1$ $b2$ >> ->
                let* << one >> = << equals $a1$ $a2$ >> in
                << equals $b1$ $b2$ >>
            | << $id:x$ >> -> failwith "types not equal"
          end
    $.

  infer : {M : tm} inf M = $ fun m ->
    Debug.log_open "infer" "%a" SLF.Printer.term m;
    let r = match* m with
      | << lam $a$ $m$ >> ->
          let* << ex $_$ $b$ $d$ >> in <:env< x:tm; h:is x $a$ >> =
            << infer ($m$ (get x (ex x $a$ h))) >> in
          << ex (lam $a$ $m$) (arr $a$ $b$) (is_lam $m$ $a$ $b$ ([x] [h] $d$)) >>
      | << app $m$ $n$ >> ->
          let* << ex $_$ $c$ $d1$ >> = << infer $m$ >> in
          let* << ex $_$ $a'$ $d2$ >> = << infer $n$ >> in
          begin match* c in <:env< >> with
            | << arr $a$ $b$ >> ->
                let* << one >> = << equals $a$ $a'$ >> in
                << ex (app $m$ $n$) $b$ (is_app $m$ $n$ $a$ $b$ $d1$ $d2$) >>
            | << $id:x$ >> -> failwith "non-functional application"
          end
      | << get $x$ $i$ >> -> i
      | << o >> -> << ex o nat is_o >>
      | << s $m$ >> ->
          let* << ex $_$ nat $d$ >> = << infer $m$ >> in
          << ex (s $m$) nat (is_s $m$ $d$) >>
      | << tt >> -> << ex tt bool is_tt >>
      | << ff >> -> << ex ff bool is_ff >>
      | << ifb $m$ $n$ $p$ >> ->
          let* << ex $_$ $b$ $d$ >> = << infer $m$ >> in
          let* << ex $_$ $a$ $d1$ >> = << infer $n$ >> in
          let* << ex $_$ $a'$ $d2$ >> = << infer $p$ >> in
          let* << one >> = << equals $b$ bool >> in
          let* << one >> = << equals $a$ $a'$ >> in
          << ex (ifb $m$ $n$ $p$) $a$ (is_if $m$ $n$ $p$ $a$ $d$ $d1$ $d2$) >>
      | << letb $m$ $n$ >> ->
          let* << ex $_$ $a$ $d1$ >> = << infer $m$ >> in
          let* << ex $_$ $b$ $d2$ >> in <:env< x:tm; h:is x $a$ >> =
            << infer ($n$ (get x (ex x $a$ h))) >> in
          << ex (letb $m$ $n$) $b$ (is_let $m$ $n$ $a$ $b$ $d1$ ([x] [h] $d2$)) >>
      | << recb $m$ $n$ $p$ >> ->
          let* << ex $_$ nat $dm$ >> = << infer $m$ >> in
          let* << ex $_$ $a$ $dn$ >> = << infer $n$ >> in
          let* << ex $_$ $a'$ $dp$ >> in <:env< x:tm; hx:is x $a$; y:tm; hy:tm >> =
            << infer ($p$ (get x (ex x nat hx)) (get y (ex y $a$ hy))) >> in
          let* << one >> = << equals $a$ $a'$ >> in
          << ex (recb $m$ $n$ $p$) $a$ (is_rec $m$ $n$ $p$ $a$ $dm$ $dn$ [x] [y] [hx] [hy] $dp$) >>
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

Tests.commit repo
<< infer (recb o o [x] [_] s x) >>
;;

Tests.commit repo
<<
  infer (
    lam nat [x] lam nat [y] recb x y [z] [_] s z
  )
>>
;;

Tests.commit repo
<<
  infer (
    letb (lam nat [x] lam nat [y] recb x y [z] [_] s z) [add]
    letb (lam nat [x] lam nat [y] recb o y [z] [_] app (app add x) z) [mult]
    letb (lam nat [x] lam nat [y] recb (s o) y [z] [_] app (app mult x) z) [exp]
    letb (lam nat [x] recb o x [_] [w] w) [pred]
    app (app exp (s o)) (s o)
  )
>>
;;

42