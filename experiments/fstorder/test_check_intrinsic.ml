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

  is : tp -> type.
  is_app : {A:tp} {B:tp}
    is (arr A B) -> is A -> is B.
  is_lam : {A:tp} {B:tp}
    (is A -> is B) -> is (arr A B).

  inf : type.
  ex : {A : tp} {H : is A} inf.

  equals : tp -> tp -> unit = $ fun a b ->
    match* a with
      | << $id:x$ >> ->
          begin match* b with
            | << $id:y$ >> when x=y -> return << one >> (* TODO: equality *)
            | << arr $_$ $_$ >> -> failwith "types not equal"
          end
      | << arr $a1$ $a2$ >> ->
          begin match* b with
            | << arr $b1$ $b2$ >> ->
                let* << one >> = << equals $a1$ $a2$ >> in
                return << equals $b1$ $b2$ >>
            | << $id:_$ >> -> failwith "types not equal"
          end
    $.

  get : {A : tp} is A -> tm = $ fun _ p ->
    match* p with
      | << is_app $a$ $b$ $p$ $q$ >> ->
          return << app (get (arr $a$ $b$) $p$) (get $a$ $q$) >>
      | << is_lam $a$ $_$ $p$ >> ->
          return << lam $a$ (get $p$) >>
      | << $id:x$ >> ->
          return << $id:x$ >>
  $.

  infer : tm -> inf = $ fun m ->
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
      | << o >> -> return << ex o nat is_o >>
      | << s $m$ >> ->
          let* << ex $_$ nat $d$ >> = << infer $m$ >> in
          return << ex (s $m$) nat (is_s $m$ $d$) >>
      | << tt >> -> return << ex tt bool is_tt >>
      | << ff >> -> return << ex ff bool is_ff >>
      | << ifb $m$ $n$ $p$ >> ->
          let* << ex $_$ $b$ $d$ >> = << infer $m$ >> in
          let* << ex $_$ $a$ $d1$ >> = << infer $n$ >> in
          let* << ex $_$ $a'$ $d2$ >> = << infer $p$ >> in
          let* << one >> = << equals $b$ bool >> in
          let* << one >> = << equals $a$ $a'$ >> in
          return << ex (ifb $m$ $n$ $p$) $a$ (is_if $m$ $n$ $p$ $a$ $d$ $d1$ $d2$) >>
      | << letb $m$ $n$ >> ->
          let* << ex $_$ $a$ $d1$ >> = << infer $m$ >> in
          let* << ex $_$ $b$ $d2$ >> in <:env< x:tm; h:is x $a$ >> =
            << infer ($n$ (infer^0 x (ex x $a$ h))) >> in
          return << ex (letb $m$ $n$) $b$ (is_let $m$ $n$ $a$ $b$ $d1$ ([x] [h] $d2$)) >>
      | << recb $m$ $n$ $p$ >> ->
          let* << ex $_$ nat $dm$ >> = << infer $m$ >> in
          let* << ex $_$ $a$ $dn$ >> = << infer $n$ >> in
          let* << ex $_$ $a'$ $dp$ >> in <:env< x:tm; hx:is x nat; y:tm; hy:is y $a$ >> =
            << infer ($p$ (infer^0 x (ex x nat hx)) (infer^0 y (ex y $a$ hy))) >> in
          let* << one >> = << equals $a$ $a'$ >> in
          return << ex (recb $m$ $n$ $p$) $a$
            (is_rec $m$ $n$ $p$ $a$ $dm$ $dn$ [x] [y] [hx] [hy] $dp$) >>
    in
    Debug.log_close "infer" "=> %a in %a" SLF.Printer.term r SLF.Printer.repo_light repo;
    repo, r
  $.

  (* red_lam : {M : tm -> tm} {N : tm} {A : tp} {B : tp} *)
  (*            is (lam A [x] M x) (arr A B) -> is N A -> *)
  (*            is (M N) B = $ fun _ n _ _ hm hn -> *)
  (*   match* hm with *)
  (*     | << is_lam $m$ $a$ $b$ $h$ >> -> return << $h$ $n$ $hn$ >> *)
  (* $. *)

  (* red_let : {M : tm -> tm} {N : tm} {A : tp} {B : tp} *)
  (*            is (letb N [x] M x) A -> is (M N) A = $ fun m n a b hl -> *)
  (*   match* hl with *)
  (*     | << is_let $m$ $n$ $a$ $b$ $hm$ $h$ >> -> *)
  (*         return << $h$ $n$ $hm$ >> *)
  (* $. *)

>>
;;

let repo = Tests.commit repo
<<
  infer (recb (s o) (s o) [x] [y] s x)
>>
;;

42
