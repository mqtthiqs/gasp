#use "load.ml"

let repo = Slicer.init
<:sign<
  A : type.
  B : type.
  P : B -> type.
  #a : A.
  #b : A.
  f : A -> A -> A -> B.

  p : {x : A} {y : A} P (f x y x).
  Q : P (f a b a) -> type.
  (* q : Q (p b a). *)
>>
;;

#trace Kernel.Check.obj
#trace LF.Subst.fam

let repo = Slicer.commit repo
<<
  p a b
>>
;;

33
