
Inductive type := Arr (A B : type).

Notation "A # B" := (Arr A B) (at level 80, right associativity).

Inductive term : type -> Type :=
| K {A B} : term (A#B#A)
| S {A B C} : term ((A#B#C)#(A#B)#(A#C))
| App A {B} : term (A#B) -> term A -> term B.

Notation "A @@ B [ C ]" := (App C A B) (at level 65, left associativity).
Notation "A @ B" := (App _ A B) (at level 60, left associativity).

Inductive env := 
| Cons A (t : term A) (tl : env)
| Nil.

Notation "t ~ A ; B" := (Cons A t B) (at level 55, right associativity).
Notation "[]" := Nil.

Fixpoint nth n E : option { A : type & term A } := 
  match n,E with
  | 0, Cons A t _ => Some (existT _ A t)
  | Datatypes.S m, Cons _ _ E => nth m E
  | _, _ => None
  end.

Notation "E |= n := A" := (nth n E = Some A) (at level 60).
Notation "E |= n :!" := (nth n E = None) (at level 60).

Reserved Notation "E |- t :: A ===> u :: B" (at level 90, A at level 80, B at level 80).
Reserved Notation "E |- t :: A" (at level 90, A at level 80, B at level 80).

Inductive diff : forall E A t B u, Type :=
| Id E A t : 
  E |- t :: A ===> t :: A
| Unload E A B t u :
  t~A ; E |- u :: B ->
  E |- t :: A ===> u :: B
| Pi E A B C (t:term (A#B)) (u:term A) v:
  u~A ; E |- t :: A#B ===> v :: C ->
  E |- t@@u[A] :: B ===> v :: C
| In1 E A B C t u v :
  E |- u :: A ->
  E |- t@u :: B ===> v :: C ->
  E |- t :: A#B ===> v :: C
| In2 E A B C t u v :
  E |- t :: A#B ->
  E |- t@u :: B ===> v :: C ->
  E |- u :: A ===> t@u :: B

where "E |- t :: A ===> u :: B " := (diff E A t B u)

with diff_u : forall E A t, Type :=
| Load n E A B t u :
  E |= n := (existT _ A t) ->
  E |- t :: A ===> u :: B ->
  E |- u :: B
(*| Appl E A B t u :
  E |- t :: A#B ->
  E |- u :: A ->
  E |- t@u :: B*)

where "E |- t :: A" := (diff_u E A t).

Hint Constructors diff diff_u : diff.

Notation id := (Id _ _ _).
Notation un := (Unload _ _ _ _ _).
Notation pi1 := (Pi _ _ _ _ _ _ _).
Notation i1 := (In1 _ _ _ _ _ _ _).
Notation i2 := (In2 _ _ _ _ _ _ _).
Notation "'l' n" := (Load n _ _ _ _ _ (refl_equal _)) (at level 10).

Ltac a := info eauto with diff.
Ltac id := eapply Id.
Ltac un := eapply Unload.
Ltac pi1 := eapply Pi.
Ltac i1 := eapply In1.
Ltac i2 := eapply In2.
Ltac lo n := eapply (Load n) ; [reflexivity | idtac].
(*Ltac ap := eapply Appl.*)

Definition SK A B C : env := S ~ (A#B#C)#(A#B)#A#C ; K ~ A#B#A ; [].

Definition permut E A B C D t u v w: 
  t ~ A ; u ~ B ; E |- v :: C ===> w :: D ->
  u ~ B ; t ~ A ; E |- v :: C ===> w :: D.
intros.
induction H. id. un.
Admitted.

Definition weak E A B C t u v :
  E |- u :: B ===> v :: C ->
  t ~ A ; E |- u :: B ===> v :: C.
intros. induction H. id. un.
Admitted.

Definition weak_u E A B t u :
  E |- u :: B ->
  t ~ A ; E |- u :: B.
Admitted.

Definition Pi2 E A B C t u v : 
   t ~ A#B ; E |- u :: A ===> v :: C ->
   E |- t@u :: B ===> v :: C.
intros. pi1. un. lo 1. apply permut.
apply weak. exact H.
Defined.

Ltac pi2 := eapply Pi2.

Section SK.

Variables X Y Z : type.
Definition SK' := SK X Y Z.

Definition replace A t B u :
  SK' |- t :: A ===> u :: B.
intros. un. unfold SK', SK.
induction u.
lo 2. admit. 
lo 1. admit.
admit.
Defined.

Definition Kxy_x A x B (y:term B) :
  [] |- K @ x @ y :: A ===> x :: A.
intros.
pi1. pi2. id.
Defined.

Definition Sxyz_xzyz A B C (x:term (A#(A#B)#C)) y z :
  [] |- S@x@y@z :: C ===> x@z@(y@z) :: C.
intros.
pi1; pi1; pi1. un.
lo 1. i1. lo 3. id.
un. lo 3. i1. lo 4. id.
i2. lo 0. id. id.
Defined.

Definition SKKt_t A t :
  [] |- S@@K[A#(A#A)#A]@K@t :: A ===> t :: A.
intros.
pi1. un. lo 1. id.
Defined.

Definition t_SKKt A t : 
  SK' |- t :: A ===> S@@K[A#(A#A)#A]@K@t :: A.
Admitted.

Definition t_StK A B C t :
  SK' |- t :: A#(B#A)#C ===> S@t@K :: A#C.
Admitted.

(* /!\ pas de partage a b *)
Definition fab_fba A B (f:term (A#A#B)) (a b:term A) :
  [] |- f@a@b :: B ===> f@b@a :: B.
intros.
pi1; pi1; i1 ; [lo 1; id| i1 ; [lo 0; id| id]].
Defined.
