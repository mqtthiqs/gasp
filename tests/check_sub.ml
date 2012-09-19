#use "load.ml"
;;

open Util

let repo = Version.init
<:sign<

  unit : type.
  one : unit.

  tp : type.
  #nat : tp.
  #even : tp.
  #odd : tp.
  arr : tp -> tp -> tp.

  tm : type.
  lam : tp -> (tm -> tm) -> tm.
  app : tm -> tm -> tm.

  sub : tp -> tp -> type.
  sub_refl: {A:tp} sub A A.
  sub_odd_nat : sub odd nat.
  sub_even_nat : sub even nat.
  sub_arr: {A:tp} {B:tp} {C:tp} {D:tp}
  sub C A ->
  sub B D -> 
  sub (arr A B) (arr C D).

  is : tm -> tp -> type.
  is_app : {M:tm} {N:tm} {A:tp} {B:tp}
    is M (arr A B) -> is N A -> is (app M N) B.
  is_lam : {M:tm -> tm} {A:tp} {B:tp}
    ({x : tm} is x A -> is (M x) B) -> is (lam A [u] M u) (arr A B).
  is_sub : {M: tm} {A:tp} {B:tp}
    sub A B ->
    is M A ->
    is M B.

  inf : tm -> type.
  ex : {M : tm} {A : tp} is M A -> inf M.

  equals : tp -> tp -> unit = $ fun a b ->
    match* a with
      | << $id:x$ >> -> 
          begin match* b with
            | << $id:y$ >> -> 
	      if x <> y then failwith "types not equal";
	      return << one >>
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
     | << arr $i$ $o$ >> ->
       begin match* b with
	 | << arr $i'$ $o'$ >> ->
	     return << sub_arr $i$ $o$ $i'$ $o'$ (subtype $i'$ $i$) (subtype $o$ $o'$) >>
       end
 $.

 infer : {M : tm} inf M = $ fun m ->
    Debug.log_open "infer" "%a" SLF.Printer.term m;

    let repo, r = match* m with
      | << lam $a$ $m$ >> ->
          let* << ex $_$ $b$ $d$ >> in <:env< x:tm; h:is x $a$ >> =
            << infer ($m$ (infer^0 x (ex x $a$ h))) >> in
          return << ex (lam $a$ $m$) (arr $a$ $b$)
            (is_lam $m$ $a$ $b$ ([x] [h] $d$))
          >>
      | << app $m$ $n$ >> ->
          let* << ex $_$ $c$ $d1$ >> = << infer $m$ >> in
          let* << ex $_$ $a'$ $d2$ >> = << infer $n$ >> in
          begin match* c in <:env< >> with
            | << arr $a$ $b$ >> ->
                return <<
                  ex (app $m$ $n$) $b$
                    (is_app $m$ $n$ $a$ $b$ $d1$
                       (is_sub $n$ $a'$ $a$ (subtype $a'$ $a$) $d2$)
                    )
                >>
            | << $id:_$ >> -> failwith "non-functional application"
          end
    in
    Debug.log_close "infer" "=> %a in %a" SLF.Printer.term r SLF.Printer.repo_light repo;
    repo, r
  $.

>>
;;

Tests.commit repo
<<
  infer (lam odd [z] z)
>>
;;

Tests.commit repo
<<
  infer (lam odd [z]
	   lam (arr nat nat) [f]
	     app f z)
>>
;;

