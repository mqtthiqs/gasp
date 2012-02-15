#use "load.ml"

#trace Kernel.Check.obj
#trace LF.Subst.fam

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
  (* q : Q (p b b). *)
>>
;;

let m = LF.Subst.obj 0
  (LF.Strat.obj repo.Repo.sign []
   << [x] [y] f x y >>)
  (match LF.Strat.obj repo.Repo.sign []
   << [f] f a b >>
   with
     | LF.OLam (_, m) -> m
     | _ -> assert false)
;;

let m = LF.Subst.obj 0
  (LF.Strat.obj repo.Repo.sign []
   << a >>)
  (match LF.Strat.obj repo.Repo.sign []
   << [x] [y] f x y >>
   with
     | LF.OLam (_, m) -> m
     | _ -> assert false)
;;

let repo = Slicer.commit repo
<<
  p a b
>>
;;

33
