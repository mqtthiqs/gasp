#use "load.ml"

let catch2 f x y =
  try f x y
  with Kernel.Conv.Not_conv (repo, m1, m2) as e ->
    Format.printf "Not convertible: @[%a@] <> @[%a@] in @[%a@]@."
      LF.Printer.obj m1 LF.Printer.obj m2 Repo.Printer.t repo;
    raise e

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

(* a term *)

let repo = Slicer.commit repo
<<
  lam [x] lam [y] app (app x y) y
>>
;;

let m = Slicer.checkout repo
;;

(* a derivation *)

let repo = Slicer.commit repo
<<
  is_lam ([x] x) base base ([x] [H] H)
>>
;;

#trace Kernel.Check.obj
#trace LF.Subst.obj
#trace LF.Subst.fam

let repo = Slicer.commit repo
<<
  is_lam ([x] app x x) base base
  [x] [H] sorry (app x x) base
>>
;;

let repo = Slicer.commit repo
<<
  is_lam ([x] lam [y] y) (arr base base) base
  [x] [H1] is_lam ([y] x) base base ([y] [H2] H2)
>>
;;

33
