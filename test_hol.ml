#use "load.ml"
;;

open Util

let repo = Version.init
<:sign<

  tp : type.
  i : tp.
  o : tp.
  arr : tp -> tp -> tp.

  obj : tp -> type.
  O : obj(i).
  S : obj(arr i i).
  plus : obj(arr i (arr i i)).
  mult : obj(arr i (arr i i)).

  lt : obj(arr i (arr i o)).

  neg : obj(arr o i).
  conj : obj(arr o (arr o o)).
  disj : obj(arr o (arr o o)).
  imp : obj(arr o (arr o o)).

  eq : {s : tp} obj(arr s (arr s o)).
  forall : {s : tp} obj(arr (arr s o) o).
  exist : {s : tp} obj(arr (arr s o) o).

  app : {s : tp} {t : tp} obj(arr s t) -> obj s -> obj t.
  lam : {s : tp} {t : tp} (obj s -> obj t) -> obj (arr s t).

  is_true : obj(o) -> type.
  True : obj(o).
  tt : is_true(True).

  all_i : {s : tp} {F : obj(arr s o)}
            ({x : obj s} is_true(app s o F x)) ->
              is_true(app (arr s o) o (forall s) F).
  all_e : {s : tp} {F : obj(arr s o)}
            {x : obj s} is_true(app (arr s o) o (forall s) F) ->
              is_true(app s o F x).

>>
;;

42
