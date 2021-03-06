#use "load.ml"
;;

let repo = Version.init
<:sign<

(* % System F *)
(* % *)
(* % Lambda-tree version, intrisic terms, à la Church. *)
(* % *)

ty : type.
arr : ty -> ty -> ty.
all : (ty -> ty) -> ty.

tm : ty -> type.
app : {A : ty} {B : ty} tm (arr A B) -> tm A -> tm B.
lam : {A : ty} {B : ty} (tm A -> tm B) -> tm (arr A B).
tapp : {A : ty -> ty} {B : ty} tm (all [a] A a) -> tm (A B).
tlam : {A : ty -> ty} ({a : ty} tm (A a)) -> tm (all [a] A a).

>>
;;
