#use "load.ml"

#trace Kernel.Check.obj
#trace Kernel.Conv.obj
#trace LF.Subst.obj
#trace LF.Subst.fam
#trace LF.Lift.fam

(* HOAS STLC *)
let repo = Slicer.init
<:sign<
  tp : type.
  #base : tp.
  arr : tp -> tp -> tp.
  tm : type.
  lam : (tm -> tm) -> tm.
  app : tm -> tm -> tm.
  is : tm -> tp -> type.
  is_app : {M:tm} {N:tm} {A:tp} {B:tp}
    is M (arr A B) -> is N A -> is (app M N) B.
  is_lam : {M:tm -> tm} {A:tp} {B:tp}
    ({x : tm} is x B -> is (M x) A) -> is (lam [a] M a) (arr A B).
  sorry : {M : tm} {A: tp} is M A.
>>
;;

let test_commit repo m =
  let repo = Slicer.commit repo m in
  let m = LF.Strat.obj repo.Repo.sign [] m in
  let n = LF.Strat.obj repo.Repo.sign [] (Slicer.checkout repo) in
  Kernel.Conv.obj repo (m, n)
;;

(* a term *)
test_commit repo
<<
  lam [x] lam [y] app (app x y) y
>>
;;

(* some derivations *)
test_commit repo
<<
  is_lam ([x] x) base base ([x] [H] H)
>>
;;

test_commit repo
<<
  is_lam ([x] app x x) base base
  [x] [H] sorry (app x x) base
>>
;;

test_commit repo
<<
  is_lam ([x] lam [y] y) (arr base base) base
  [x] [H1] is_lam ([y] x) base base ([y] [H2] H2)
>>
;;

42
