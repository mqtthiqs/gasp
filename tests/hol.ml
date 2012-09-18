#use "load.ml"
;;

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
times : obj(arr i (arr i i)).

lt : obj(arr i (arr i o)).

not : obj(arr o i).
conj : obj(arr o (arr o o)).
disj : obj(arr o (arr o o)).
imp : obj(arr o (arr o o)).

eq : {s : tp} obj(arr s (arr s o)).
forall : {s : tp} obj(arr (arr s o) o).
exist : {s : tp} obj(arr (arr s o) o).

app : {s : tp} {t : tp} obj(arr s t) -> obj s -> obj t.
lam : {s : tp} {t : tp} (obj s -> obj t) -> obj (arr s t).

pf : obj(o) -> type.
True : obj(o).
tt : pf(True).

all_i : {s : tp} {F : obj(arr s o)}
	 ({x : obj s} pf(app s o F x)) -> 
	 pf(app (arr s o) o (forall s) F).
all_e : {s : tp} {F : obj(arr s o)}
	 {x : obj s} pf(app (arr s o) o (forall s) F) -> pf(app s o F x).

>>
;;
