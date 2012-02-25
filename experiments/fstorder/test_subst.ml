#use "load.ml"

#trace Kernel.Check.obj
#trace LF.Subst.fam
#trace LF.Subst.obj
#trace LF.Subst.kind
#trace LF.Subst.spine
#trace LF.Lift.obj
;;

let repo = Slicer.init
<:sign<
  A : type.
  B : type.
  P : B -> type.
  #a : A.
  #b : A.
  f : A -> A -> A -> B.

  p : {x : A} {y : A} P (f x y x).
  Q : P (f a b b) -> type.
>>
;;

let repo = Slicer.commit repo
<<
  p a b
>>
;;

(* ([x] n) [p <- m] *)
let test_subst m n p =
  let p = LF.Strat.obj repo.Repo.sign [] p in
  let m = LF.Strat.obj repo.Repo.sign [] m in
  let n = match LF.prj (LF.Strat.obj repo.Repo.sign [] n) with
    | LF.OLam (_, n) -> n
    | _ -> assert false in
  let q = LF.Subst.obj [m] n in
  Kernel.Conv.obj repo (p, q)
;;

test_subst
<< a >>
<< [X] X >>
<< a >>
;;

test_subst
<< a >>
<< [X] [y] f X y >>
<< [y] f a y >>
;;

test_subst
<< [x] [y] x >>
<< [X] X a b >>
<< a >>
;;

test_subst
<< [x] [y] f x y >>
<< [X] X a b >>
<< f a b >>
;;

test_subst
<< [x] x >>
<< [X] [y] X y>>
<< [x] x >>
;;

test_subst
<< [x] [y] x >>
<< [X] [z] X z a >>
<< [z] z >>
;;

test_subst
<< [a] ?X[a] >>
<< [X] [x] X x >>
<< [x] ?X[x] >>
;;

