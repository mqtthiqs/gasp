
Inductive type := Arr (A B : type).

Notation "A # B" := (Arr A B) (at level 80, right associativity).

Inductive term : type -> Type :=
| K {A B} : term (A#B#A)
| S {A B C} : term ((A#B#C)#(A#B)#(A#C))
| App A {B} : term (A#B) -> term A -> term B.

Notation "A @@ B [ C ]" := (App C A B) (at level 65, left associativity).
Notation "A @ B" := (App _ A B) (at level 60, left associativity).
Reserved Notation "|- t :: A ===> u :: B" (at level 0, A at level 80, B at level 80).

Inductive diff : forall T, term T -> forall U, term U -> Type :=
| Id T t : 
  |- t :: T ===> t :: T
| Bot T U t u :
  |- t :: T ===> u :: U
| App_ctx A B C D t u v w : 
  |- t :: A#C ===> v :: B#D ->
  |- u :: A ===> w :: B ->
  |- t@u:: C ===> v@w:: D
| Intro1 A B t u :
  |- u :: A ===> t@u :: B
| Intro2 A B t u :
  |- t :: A#B ===> t@u :: B
| Elim1 A B t u :
  |- t@@u[A] :: B ===> u :: A
| Elim2 A B t u :
  |- t@@u[A] :: B ===> t :: A#B
| Trans {A t} B u {C v} :
  |- t :: A ===> u :: B ->
  |- u :: B ===> v :: C ->
  |- t :: A ===> v :: C

where "|- t :: A ===> u :: B " := (diff A t B u).

Notation "A ; B" := (Trans _ _ A B) (at level 55, right associativity).
Notation e1 := (Elim1 _ _ _ _).
Notation e2 := (Elim2 _ _ _ _).
Notation i1 := (Intro1 _ _ _ _ ).
Notation i2 := (Intro2 _ _ _ _ ).
Notation id := (Id _ _).
Notation bot := (Bot _ _ _ _).
Notation app := (App_ctx _ _ _ _ _ _ _ _).

Hint Constructors diff : diff.

Ltac a := info eauto with diff.
Ltac e1 := eapply Elim1.
Ltac e2 := eapply Elim2.
Ltac i1 := eapply Intro1.
Ltac i2 := eapply Intro2.
Ltac app := eapply App_ctx.
Ltac id := eapply Id.
Ltac bot := eapply Bot.
Ltac tr := eapply Trans.

Definition replace A t B u :
  |- t :: A ===> u :: B := bot.

Definition Kxy_x A x B (y:term B) :
  |- K @ x @ y :: A ===> x :: A :=
e2 ; e1.

Definition Sxyz_xzyz A B C (x:term (A#(A#B)#C)) y z :
  |- S@x@y@z :: C ===> x@z@(y@z) :: C :=
app bot i1.

Definition t_SKKt A t : 
  |- t :: A ===> S@@K[A#(A#A)#A]@K@t :: A :=
i1.

Definition t_StK A B C t :
  |- t :: A#(B#A)#C ===> S@t@K :: A#C :=
i1 ; i2.

(* /!\ pas de partage a b *)
Definition fab_fba A B (f:term (A#A#B)) (a b:term A) :
  |- f@a@b :: B ===> f@b@a :: B := 
app (app id bot) bot.

