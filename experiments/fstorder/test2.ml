#use "load.ml"

(* HOAS STLC *)
let stlc = Slicer.init
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
>>
;;

(* a term *)

(* let m = *)
(* << *)
(*   lam [x] lam [y] app (app x y) y *)
(* >> *)
(* ;; *)

(* let repo = Slicer.commit stlc m *)
(* ;; *)

(* let m = Slicer.checkout repo *)
(* ;; *)

(* a derivation *)

#trace Kernel.Check.obj
#trace LF.Subst.fam
#trace LF.Subst.obj
#trace LF.Subst.kind

let repo = Slicer.commit stlc
<<
  is_lam ([x] x) base base ([x] [H] H)
>>
;;

33
