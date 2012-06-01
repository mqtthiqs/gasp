#use "load.ml"
;;

(* Goedel's T with subtyping (even, odd <= nat) *)

open Util
;;

(* Debug.tags := [] *)
(* ;; *)

let repo = Version.init
<:sign<

  Bool : type.
  True : Bool.
  False : Bool.

  tp : type.
  #bool : tp.
  #nat : tp.
  #even : tp.
  #odd : tp.
  arr : tp -> tp -> tp.

  tm : type.
  lam : tp -> (tm -> tm) -> tm.
  app : tm -> tm -> tm.
  #o : tm.
  s : tm -> tm.
  #tt : tm.
  #ff : tm.
  letb : tm -> (tm -> tm) -> tm.
  ifb : tm -> tm -> tm -> tm.
  recb : tm -> tm -> (tm -> tm -> tm) -> tm.

  sub : tp -> tp -> type.
  sub_refl: {A:tp} sub A A.
  #sub_odd_nat : sub odd nat.
  #sub_even_nat : sub even nat.
  sub_arr: {A:tp} {B:tp} {C:tp} {D:tp}
    sub C A -> sub B D -> sub (arr A B) (arr C D).

  is : tm -> tp -> type.
  is_app : {M:tm} {N:tm} {A:tp} {B:tp}
    is M (arr A B) -> is N A -> is (app M N) B.
  is_lam : {M:tm -> tm} {A:tp} {B:tp}
    ({t : tm} is t A -> is (M t) B) -> is (lam A [u] M u) (arr A B).
  is_o : is o even.
  is_se : {M:tm} is M odd -> is (s M) even.
  is_so : {M:tm} is M even -> is (s M) odd.
  is_sn : {M:tm} is M nat -> is (s M) nat.
  #is_tt : is tt bool.
  #is_ff : is ff bool.
  is_if : {M : tm} {N1 : tm} {N2 : tm} {A : tp}
    is M bool -> is N1 A -> is N2 A -> is (ifb M N1 N2) A.
  is_let : {M : tm} {N : tm -> tm} {A : tp} {B : tp}
    is M A -> ({x : tm} is x A -> is (N x) B) -> is (letb M [x] N x) B.
  is_rec : {M : tm} {N : tm} {P : tm -> tm -> tm} {A : tp}
    is M nat ->
    is N A ->
    ({x:tm} {y:tm} is x nat -> is y A -> is (P x y) A) ->
    is (recb M N [x] [y] P x y) A.
  is_sub : {M: tm} {A:tp} {B:tp}
    sub A B ->
    is M A ->
    is M B.

  inf : tm -> type.
  ex : {M : tm} {A : tp} {H : is M A} inf M.

  prj : {M : tm} {A : tp} inf M -> is M A = $ fun _ _ i ->
    match* i with
      | << ex $_$ $_$ $h$ >> -> return h
  $.

  equals : tp -> tp -> Bool = $ fun a b ->
    match* a with
      | << $id:x$ >> ->
          begin match* b with
            | << $id:y$ >> when x=y -> return << True >> (* TODO: equality *)
            | << $id:_$ >> | << arr $_$ $_$ >> -> return << False >>
          end
      | << arr $a1$ $a2$ >> ->
          begin match* b with
            | << arr $b1$ $b2$ >> ->
                begin match* << equals $a1$ $a2$ >> with
                  | << True >> -> return << equals $b1$ $b2$ >>
                  | << False >> -> return << False >>
                end
            | << $id:_$ >> ->  return << False >>
          end
    $.

  subtype : {A : tp} {B : tp} sub A B = $ fun a b ->
    match* a with
      | << nat >> ->
          begin match* b with
	    | << nat >> -> return << sub_refl nat >>
	    | << odd >> | << even >> | << arr $_$ $_$ >> ->
	        failwith "subtype error"
          end
      | << odd >> ->
          begin match* b with
	    | << nat >> -> return << sub_odd_nat >>
	    | << odd >> -> return << sub_refl odd >>
	    | << even >> | << arr $_$ $_$ >> ->
	        failwith "subtype error"
          end
      | << even >> ->
          begin match* b with
	    | << nat >> -> return << sub_even_nat >>
	    | << even >> -> return << sub_refl even >>
	    | << odd >> | << arr $_$ $_$ >> ->
	        failwith "subtype error"
          end
      | << bool >> ->
          begin match* b with
            | << bool >> -> return << sub_refl bool >>
            | << nat >> | << even >> | << odd >> ->
                failwith "subtype error"
          end
      | << arr $i$ $o$ >> ->
          begin match* b with
	    | << arr $i'$ $o'$ >> ->
	       return << sub_arr $i$ $o$ $i'$ $o'$ (subtype $i'$ $i$) (subtype $o$ $o'$) >>
            | << nat >> | << even >> | << odd >> | << bool >> ->
                failwith "subtype error"
          end
  $.

  maybe_is_sub : {M : tm} {A : tp} {B : tp} is M A -> is M B = $ fun m a b d ->
    match* << subtype $a$ $b$ >> with
      | << sub_refl $_$ >> -> return d
      | (<< sub_arr $_$ $_$ $_$ $_$ $_$ $_$ >> | << sub_odd_nat >> | << sub_even_nat >>) as s ->
          return << is_sub $m$ $a$ $b$ $s$ $d$ >>
  $.

  sup : tp -> tp -> tp = $ fun a b ->
    match* a with
      | << bool >> ->
          begin match* b with
            | << bool >> -> return << bool >>
            | << nat >> | << even >> | << odd >> -> failwith "no sup"
          end
      | << nat >> ->
          begin match* b with
            | << bool >> -> failwith "no sup"
            | << nat >> | << even >> | << odd >> -> return << nat >>
          end
      | << even >> ->
          begin match* b with
            | << bool >> -> failwith "no sup"
            | << even >> -> return << even >>
            | << nat >> | << odd >> -> return << nat >>
          end
      | << odd >> ->
          begin match* b with
            | << bool >> -> failwith "no sup"
            | << odd >> -> return << odd >>
            | << nat >> | << even >> -> return << nat >>
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
                return << ex (app $m$ $n$) $b$
                  (is_app $m$ $n$ $a$ $b$ $d1$
                     (maybe_is_sub $n$ $a'$ $a$ $d2$)
                  )
                >>
            | << $id:_$ >> -> failwith "non-functional application"
          end
      | << o >> -> return << ex o even is_o >>
      | << s $m$ >> ->
          let* << ex $_$ $a$ $d$ >> = << infer $m$ >> in
          begin match* a with
            | << nat >> -> return << ex (s $m$) nat (is_sn $m$ $d$) >>
            | << even >> -> return << ex (s $m$) odd (is_so $m$ $d$) >>
            | << odd >> -> return << ex (s $m$) even (is_se $m$ $d$) >>
          end
      | << tt >> -> return << ex tt bool is_tt >>
      | << ff >> -> return << ex ff bool is_ff >>
      | << ifb $m$ $n$ $p$ >> ->
          let* << ex $_$ $tm$ $dm$ >> = << infer $m$ >> in
          let* << ex $_$ $tn$ $dn$ >> = << infer $n$ >> in
          let* << ex $_$ $tp$ $dp$ >> = << infer $p$ >> in
          let* << $a$ >> = << sup $tn$ $tp$ >> in
          return << ex (ifb $m$ $n$ $p$) $a$
            (is_if $m$ $n$ $p$ $a$
               (maybe_is_sub $m$ $tm$ bool $dm$)
               (maybe_is_sub $n$ $tn$ $a$ $dn$)
               (maybe_is_sub $p$ $tp$ $a$ $dp$)
            )
          >>
      | << letb $m$ $n$ >> ->
          let* << ex $_$ $a$ $d1$ >> = << infer $m$ >> in
          let* << ex $_$ $b$ $d2$ >> in <:env< x:tm; h:is x $a$ >> =
            << infer ($n$ (infer^0 x (ex x $a$ h))) >> in
          return << ex (letb $m$ $n$) $b$ (is_let $m$ $n$ $a$ $b$ $d1$ ([x] [h] $d2$)) >>
      | << recb $m$ $n$ $p$ >> ->
          let* << ex $_$ $tm$ $dm$ >> = << infer $m$ >> in
          let* << ex $_$ $tn$ $dn$ >> = << infer $n$ >> in
          let* << ex $_$ $tp$ $_$ >> in <:env< x:tm; hx:is x nat; y:tm; hy:is y $tn$ >> =
            << infer ($p$ (infer^0 x (ex x nat hx)) (infer^0 y (ex y $tn$ hy))) >> in
          let* << $a$ >> = << sup $tn$ $tp$ >> in
          let* << ex $_$ $tp$ $dp$ >> in <:env< x:tm; hx:is x nat; y:tm; hy:is y $a$ >> =
            << infer ($p$ (infer^0 x (ex x nat hx)) (infer^0 y (ex y $a$ hy))) >> in
          return << ex (recb $m$ $n$ $p$) $a$
            (is_rec $m$ $n$ $p$ $a$
               (maybe_is_sub $m$ $tm$ nat $dm$)
               (maybe_is_sub $n$ $tn$ $a$ $dn$)
               [x] [y] [hx] [hy] (maybe_is_sub ($p$ x y) $tp$ $a$ $dp$)) >>
    in
    Debug.log_close "infer" "=> %a in %a" SLF.Printer.term r SLF.Printer.repo_light repo;
    repo, r
  $.

>>
;;

42
