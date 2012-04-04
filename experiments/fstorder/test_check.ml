#use "load.ml"
;;

open Util

let equals repo env m n a =
  let m = SLF.Strat.obj repo.Struct.Repo.sign [] m in
  let n = SLF.Strat.obj repo.Struct.Repo.sign [] n in
  let a = SLF.Strat.fam repo.Struct.Repo.sign [] a in
  Kernel.Conv.obj repo env (m, n, a)

let repo = Slicer.init
<:sign<

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

  get : {M : tm} inf M -> tm = $fun m _ -> m$.

  infer : {M : tm} inf M = $ fun m ->
    Debug.log_open "infer" "%a" SLF.Printer.term m;
    let r = match m rec Kernel.eval repo env with
      | << lam $a$ $m$ >> ->
          begin match infer << $m$ (get x (ex x $a$ h)) >> rec Kernel.eval repo env with
            | << [$x$] [$h$] ex $_$ $b$ $d$ >> ->
                << is_lam ([$x$] $m$) $a$ $b$ ([$x$] [$h$] $d$) >>
          end
      | << app $m$ $n$ >> ->
          begin match infer m rec Kernel.eval repo env with
            | << ex $_$ (arr $a$ $b$) $d1$ >> ->
                match infer n rec Kernel.eval repo env with
                  | << ex $_$ $a'$ $d2$ >> ->
                      equals repo [] a a' << tp >>;
                      << is_app $m$ $n$ $a$ $b$ $d1$ $d2$ >>
          end
      | << get $x$ (ex $_$ $a$ $h$) >> -> h
    in
    Debug.log_open "infer" "=> %a" SLF.Printer.term m;
    r
  $.

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
  infer (get (lam [x] x)
           (ex (lam [x] x) (arr base base)
              (is_lam ([x] x) base base ([_] [H] H))))
>>
;;

42
