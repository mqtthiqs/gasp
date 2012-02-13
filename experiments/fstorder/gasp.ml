(* First-order STLC *)
(* let stlc = Slicer.init *)
(* <:sign< *)
(*   tp : type. *)
(*   base : tp. *)
(*   arr : tp -> tp -> tp. *)

(*   tm : type. *)
(*   lam : tm -> tm. *)
(*   #app : tm -> tm -> tm. *)
(*   s : tm -> tm. *)
(*   z : tm. *)

(*   env : type. *)
(*   nil : env. *)
(*   cons : tp -> env -> env. *)

(*   is : env -> tm -> tp -> type. *)

(*   is_weak : {E:env} {M:tm} {A:tp} {B:tp} *)
(*     is E M B -> is (cons A E) (s M) B. *)
(*   is_var : {E:env} {M:tm} {A:tp} *)
(*     is (cons A E) z A. *)
(*   is_app : {E:env} {M:tm} {N:tm} {A:tp} {B:tp} *)
(*     is E M (arr A B) -> is E N A -> is E (app M N) B. *)
(*   is_lam : {E:env} {M:tm} {A:tp} {B:tp} *)
(*     is (cons B E) M A -> is E (lam M) A. *)
(* >> *)

(* let repo = Slicer.commit stlc *)
(*   <:obj< *)
(*     lam (lam (app (s z) z)) *)
(*   >> *)
(* ;; *)

(* Format.printf "%a " Repo.Printer.t repo *)
(* ;; *)


(* HOAS STLC *)
let stlc = Slicer.init
<:sign<
  tp : type.
  base : tp.
  arr : tp -> tp -> tp.

  tm : type.
  lam : (tm -> tm) -> tm.
  app : tm -> tm -> tm.

  is : tm -> tp -> type.

  is_app : {M:tm} {N:tm} {A:tp} {B:tp}
    is M (arr A B) -> is N A -> is (app M N) B.
  is_lam : {M:tm -> tm} {A:tp} {B:tp}
    ({x : tm} is x B -> is (M x) A) -> is (lam ([a] M a)) A.
>>
;;

let m =
<:obj<
  lam ([x] lam ([y] app (app x y) y))
>>
;;

Format.printf "~~~> Commit: %a@." SLF.Printer.term m
;;

let repo = Slicer.commit stlc m
;;

Format.printf "~~~> Repo: %a@." Repo.Printer.t repo
;;

Format.printf "~~~> Term: %a@." SLF.Printer.term (Slicer.checkout repo)
;;
