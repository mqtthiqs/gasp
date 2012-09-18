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
    | << app $e1$ $e2$ >> ->
      begin match* n with
      | << app $f1$ $f2$ >> ->
        let* d1 = << equals $e1$ $f1$ >> in
        let* d2 = << equals $e2$ $f2$ >> in
        return << eq_app $e1$ $f1$ $e2$ $f2$ $d1$ $d2$ >>
      | _ -> failwith "not equal"
      end
    | << lam $t$ >> ->
      begin match* n with
      | << lam $u$ >> ->
        let* d in <:env< x : tm; h : eq x x >> =
          << equals ($t$ (equals^0 x x h)) ($u$ (equals^1 x x h)) >> in
        return << eq_lam $t$ $u$ [x] [h] $d$ >>
      | _ -> failwith "not equal"
      end
  $.
>>
;;

Tests.commit_eq repo <<
  equals (lam [x] x) (lam [y] y)
>> <<
  eq_lam ([x] x) ([y] y) [x] [h] h
>>
;;

Tests.commit repo <<
  equals (lam [a] app a a) (lam [b] app b b)
>>
;;

Tests.commit repo <<
  equals (lam [a] lam [b] app a b) (lam [c] lam [d] app c d)
>>
;;

Tests.commit repo <<
  equals
  (lam [a] lam [b] app (app a b) (app b a))
  (lam [c] lam [d] app (app c d) (app d c))
>>
;;

42
