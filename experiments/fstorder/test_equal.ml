#use "load.ml"
;;

let repo = Version.init
<:sign<

  tm : type.
  lam : (tm -> tm) -> tm.
  app : tm -> tm -> tm.

  eq : tm -> tm -> type.
  eq_app : {E1:tm} {F1:tm} {E2:tm} {F2:tm}
    eq E1 F1 -> eq E2 F2 -> eq (app E1 E2) (app F1 F2).
  eq_lam :  {E:tm->tm} {F:tm->tm}
    ({x : tm} eq x x -> eq (E x) (F x))
    -> eq (lam [x] E x) (lam [x] F x).

  equals : {M : tm} {N : tm} eq M N = $ fun m n ->
    match* m with
    | << app $t1$ $u1$ >> ->
      begin match* n with
      | << app $t2$ $u2$ >> ->
        let* d1 = << equals $t1$ $t2$ >> in
        let* d2 = << equals $u1$ $u2$ >> in
        return << eq_app $t1$ $u1$ $t2$ $u2$ $d1$ $d2$ >>
      | _ -> failwith "not equal"
      end
    | << lam $t$ >> ->
      begin match* n with
      | << lam $u$ >> ->
        let* d in <:env< x : tm; h : eq x x >> =
          << equals ($t$ (equals^0 ($t$ x) ($u$ x) h)) ($u$ (equals^1 ($t$ x) ($u$ x) h)) >> in
        return << eq_lam $t$ $u$ [x] [h] $d$ >>
      | _ -> failwith "not equal"
      end
  $.
>>
;;

Tests.commit repo <<
  equals (lam [x] x) (lam [y] y)
>>
;;

42
