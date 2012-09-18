#use "load.ml"
;;

let repo = Version.init
<:sign<

tm : type.
lam : (tm -> tm) -> tm.
app : tm -> tm -> tm.
o : tm.
s : tm.
tt : tm.
ff : tm.
eq : tm.

ty : type.
nat : ty.
bool : ty.
arr : ty -> ty -> ty.

is : tm -> ty -> type.

is_o : is o nat.
is_s : is s (arr nat nat).
is_tt : is tt bool.
is_ff : is ff bool.
is_eq : is eq (arr nat (arr nat bool)).

is_app : {t : tm} {u : tm} {A : ty} {B : ty}

	  is t (arr A B) -> is u A ->
         (* %---------------------------- *)
		 is (app t u) B.

is_lam : {t : tm -> tm} {A : ty} {B : ty}

	  ({x : tm} is x A -> is (t x) B) ->
	  (* %--------------------------------- *)
          is (lam ([x] t x)) (arr A B).

>>
;;


let repo = Tests.commit repo <<
  lam [x] lam [y] app x (app (app x y) y)
>>
;;

let repo = Tests.commit repo <<
  lam [x] ?aajwyo4er5[x]
>>
;;

let repo = Tests.commit repo <<
  lam [x] app ?aajwyo4er5[x] (lam [z] ?m1b207nbqxfab[x;z])
>>
;;

42

