#use "load.ml"
;;

let equals repo a b =
  let a = SLF.Strat.obj repo.Struct.Repo.sign [] a in
  let b = SLF.Strat.obj repo.Struct.Repo.sign [] b in
  Kernel.Conv.obj repo (a, b)

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

  infer : {M : tm} inf M = $ function
    | << lam $a$ [$x$] $m$ >> ->
      begin match infer << ([$x$] m) (get x H) >> with
        | << ex $_$ $b$ $d$ >> -> << is_lam ([$x$] $m$) $a$ $b$ [$x$] [H] $d$ >>
        | _ -> assert false
      end
    | << app $m$ $n$ >> ->
      begin match infer m with
        | << ex $_$ (arr $a$ $b$) $d1$ >> ->
          begin match infer n with
            | << ex $_$ $a'$ $d2$ >> ->
              equals repo a a';
              << is_app $m$ $n$ $a$ $b$ $d1$ $d2$ >>
            | _ -> assert false
          end
        | _ -> assert false
      end
    | << get $x$ $h$ >> -> h
    | m ->
      SLF.Printer.term Format.std_formatter m;
      assert false
  $.
>>
;;

let test_commit repo m n =
  let repo = Slicer.commit repo m in
  let p = SLF.Strat.obj repo.Struct.Repo.sign [] (Slicer.checkout repo) in
  let n = SLF.Strat.obj repo.Struct.Repo.sign [] n in
  Kernel.Conv.obj repo (n, p);
  repo
;;

test_commit repo
<<
  infer (lam (arr base base) [x] lam base [y] app x y)
>>
<<
  base
>>
;;

42
