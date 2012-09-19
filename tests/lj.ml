#use "load.ml"
;;

exception Failure

let repo = Version.init
<:sign<
  nat : type.
  z : nat.
  s : nat -> nat.

  o : type.
  top : o.
  bot : o.
  at : nat -> o.
  imp : o -> o -> o.
  conj : o -> o -> o.
  disj : o -> o -> o.

  conc : o -> type.
  hyp : o -> type.

  id : {A:o} hyp A -> conc A.

  imp_r : {A:o} {B:o} (hyp A -> conc B) -> conc (imp A B).
  imp_l : {A:o} {B:o} {C:o} conc A -> (hyp B -> conc C) -> hyp (imp A B) -> conc C.
  top_r : conc top.
  bot_l : {C:o} hyp bot -> conc C.
  conj_r : {A:o} {B:o} conc A -> conc B -> conc (conj A B).
  conj_l1 : {A:o} {B:o} {C:o} (hyp A -> conc C) -> hyp (conj A B) -> conc C.
  conj_l2 : {A:o} {B:o} {C:o} (hyp B -> conc C) -> hyp (conj A B) -> conc C.
  disj_r1 : {A:o} {B:o} conc A -> conc (disj A B).
  disj_r1 : {A:o} {B:o} conc B -> conc (disj A B).
  disj_l : {A:o}{B:o}{C:o} (hyp A -> conc C) -> (hyp B -> conc C) -> hyp (disj A B) -> conc C.
>>
;;

(* proof of B -> B *)
let _ = Tests.commit repo <<
  imp_r bot bot [x] id bot x
>>
;;

(*
   ---------- id  ---------- id
   t : T |- T     b : B |- B
   ---------- imp_l
   tb : TB, t : T |- B
   ---------- imp_r
   TB |- TB
   ---------- imp_r
   (TB)TB
*)
(* proof of (T -> B) -> T -> B *)
let _ = Tests.commit repo <<
  imp_r (imp top bot) (imp top bot) [tb] (* tb : hyp (imp top bot) *)
    imp_r top bot [t]                    (* t : hyp top *)
      imp_l top bot bot (id top t) ([b] id bot b) tb
>>
;;

42
