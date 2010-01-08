
Require Import List.
Local Notation "[ ]" := nil : list_scope.
Local Notation "[ a ; .. ; b ]" := (a :: .. (b :: []) ..) : list_scope.

Open Scope list_scope.

Inductive type := Arr (A B : type).

Notation "A # B" := (Arr A B) (at level 80, right associativity).

Definition env := list type.

Reserved Notation "E |- A" (at level 90, A at level 80).

Inductive term : env -> type -> Type :=
| O {E A} : 
  A :: E |- A
| S {E A B} : 
  E |- B ->
  A :: E |- B
| Lam {E A B} : 
  A :: E |- B ->
  E |- A#B
| App {E A B} : 
  E |- A#B ->
  E |- A ->
  E |- B
where "E |- A" := (term E A).

Inductive p : nat -> Prop :=
| c1 : p O
| c2 : forall n, p (S n).



Notation "\ u" := (Lam u) (at level 61, left associativity).
Notation "t @ u" := (App t u) (at level 62).

Definition deriv := { E : env & { A : type & term E A } }.
Definition ctx := list deriv.
Definition mkelt `(t : E|-A) : deriv := 
  existT _ E (existT (fun A => term E A) A t).
Definition cons_ctx (D:ctx) `(t : E|-A) := (mkelt t) :: D.

Notation "t :: D" := (cons_ctx D t).
Notation "t ::: u" := (cons t u) (at level 60).

Notation "E |=== n := A" := (nth_error E n = Some A) (at level 60).

Reserved Notation "D |= t ===> u" (at level 65).
Reserved Notation "D |== t " (at level 66).

Inductive diff (D:ctx) : forall {E F A B}, term E A -> term F B -> Type :=
| Id `{t : E |- A} : 
       D |= t ===> t
| Unload `{t : E |- A} `{u : F |- B} :
  t :: D |== u ->
       D |= t ===> u
| AppE `{t : E |- (A#B)} {u} `{v : F |- C}  :
  u :: D |= t ===> v ->
       D |= t @ u ===> v
| AppI1 `{t : E |- A#B} {u} `{v : F |- C} :
       D |== u ->
       D |= t @ u ===> v ->
       D |= t ===> v
| AppI2 `{t : E |- A#B} {u} `{v : F |- C} :
       D |== t ->
       D |= t @ u ===> v ->
       D |= u ===> v
| LamE `{t : A ::: E |- B} `{u : F |- C} :
       D |= t ===> u ->
       D |= \ t ===> u
| LamI `{t : A ::: E |- B} `{u : F |- C} :
       D |= \ t ===> u ->
       D |= t ===> u

where "D |= t ===> u " := (diff D _ _ _ _ t u)

with diff_u (D:ctx) : forall {E A} (t : E |- A), Type :=
| Load n `{t: E |- A} `{u: F |- C} :
       D |=== n := mkelt t -> 
       D |= t ===> u ->
       D |== u

where "D |== t" := (diff_u D _ _ t).

Hint Constructors diff diff_u : diff.

Notation id := (Id).
Notation un := (Unload).
Notation ae1 := (AppE).
Notation ai1 := (AppI1).
Notation ai2 := (AppI2).
Notation le := (LamE).
Notation li := (LamI).
Notation "'l' n" := (Load _ n (refl_equal _)) (at level 10).

Ltac a := info eauto 6 with diff.
Ltac id := eapply Id.
Ltac un := eapply Unload.
Ltac ae1 := eapply AppE.
Ltac ai1 := eapply AppI1.
Ltac ai2 := eapply AppI2.
Ltac li := eapply LamI.
Ltac le := eapply LamE.
Ltac lo n := eapply (Load _ n) ; [simpl; reflexivity | idtac].

Hint Extern 1 => lo 0 : diff.
Hint Extern 1 => lo 1 : diff.
Hint Extern 1 => lo 2 : diff.
Hint Extern 1 => lo 3 : diff.
Hint Extern 1 => lo 4 : diff.

Lemma permut X `(t:E|-A) `(u:F|-B) `(v:G|-C) `(w:H|-D) :
  t :: u :: X |= v ===> w ->
  u :: t :: X |= v ===> w
with permut_u X `(t:E|-A) `(u:F|-B) `(v:G|-C) :
  t :: u :: X |== v ->
  u :: t :: X |== v.
Admitted.

Lemma weak X `(t:E|-A) `(u:F|-B) `(v:G|-C) :
  X |= u ===> v ->
  t :: X |= u ===> v
with weak_u X `(t:E|-A) `(u:F|-B) :
  X |== u ->
  t :: X |== u.
intros.
induction H.
id. un. apply permut_u. apply weak_u. trivial.
ae1. apply permut. trivial.
Admitted.

Hint Resolve permut weak weak_u : diff.

Definition AppE2 {D} `{t : E|-A#B} {u} `{v : F|-C} : 
  t :: D |= u ===> v ->
       D |= t @ u ===> v.
intros. ae1. un. lo 1. apply permut. apply weak. exact H.
Defined.

Notation ae2 := AppE2.
Ltac ae2 := eapply AppE2.

Definition beta `(t : A:::E|-B) `(u : E|-A) : [] |= (\ t) @ u ===> u.
intros. ae2. id.
Defined.

Definition eta `(t : A:::E|-A#B) : [] |= \ (t @ O) ===> t.
intros. le. ae1. id.
Defined.

Definition fab_fba `(f:E|-A#A#B) `(a:E|-A) `(b:E|-A) :
   [] |= f@a@b ===> f@b@a :=
ae1 (ae1 (ai1 (l 1 id) (ai1 (l 0 id) id))).

intros. ae1. ae1. ai1. [lo 1;id |]. ai1 ; [lo 0; id|]. id.

Definition church A := [] |- (A#A)#A#A.
Definition zero A : church A := \ \ O.
Definition succ A (n:church A) : (A#A) ::: (A ::: []) |- A := n @ (S O) @ O.
Definition succ A (n:church A) : church A := \ \ ((S O) @ (n @ (S O) @ O)).

