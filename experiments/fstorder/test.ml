let s =
<:raw_sign<
nat : type.
bla : nat -> type.
bla_x : {X : nat} bla X.

(* nat : type. *)
(* z : nat. *)
(* s : nat -> nat. *)

(* bla : nat -> type. *)
(* bla_x : {X : nat} bla X -> bla X. *)

(* nt : nat -> type. *)
(* nt_z : nt z. *)
(* nt_s : {X : nat} {Y : nt (s X)} nt X. *)

(* plus : nat -> nat -> nat -> type. *)
(* p_z : {Y : nat} plus z Y Y. *)
(* p_s : {X : nat} {Y : nat} {Z : nat} plus (s X) Y (s Z) *)
(*        <- plus X Y Z. *)

(* acker : nat -> nat -> nat -> type. *)

(* a_1   : {Y : nat} acker z Y (s Y). *)
(* a_2   : {X : nat} {Z : nat} acker (s X) z Z  *)
(*      <- acker X (s z) Z. *)
(* a_3   : {X : nat} {Y : nat} {Z : nat} {Z' : nat} acker (s X) (s Y) Z *)
(*      <- acker (s X) Y Z' *)
(*      <- acker X Z' Z.   *)
>>;;

let s = LF.Strat.sign LF.Sign.empty s;;
