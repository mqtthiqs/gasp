Require Import List.
Require Import Program.

Definition var := nat.

Inductive atom_name := A | B | C | D | E.

Inductive atom :=
| Atom (A : atom_name) (xs : list var).

Coercion Atom : atom_name >-> Funclass.

Definition arity_of_atom (a : atom_name) : list atom :=
  match a with
    | A => []   | B => []
    | C => [ A []]  | D => [A []; B []]
    | E => [A []; C [1]]
  end.

  
Definition env := list atom.

Reserved Notation "Γ ⊢ [ n ] = a" (at level 59, n at next level).

Inductive list_nth {T} : nat -> list T -> T -> Prop :=
| Nth_o (a:T) Γ : 
  (a :: Γ) ⊢ [0] = a

| Nth_s m (a b:T) Γ : 
  Γ ⊢ [m] = a ->
  (b :: Γ) ⊢ [S m] = a

where "Γ ⊢ [ n ] = a" := (@list_nth _ n Γ a).

Reserved Notation "Γ ⊢ a" (at level 50, a at next level).
Reserved Notation "Γ ⊢ vs ≡ As" (at level 50, vs at next level).

Inductive wt_atom (Γ : env) : atom -> Prop :=
| Wt_atom A xs :
  Γ ⊢ xs ≡ arity_of_atom A ->
  Γ ⊢ A xs
  where "Γ ⊢ a" := (wt_atom Γ a)

with wt_atoms (Γ : env) : list var -> list atom -> Prop :=
| Wt_atoms_nil :
  Γ ⊢  [] ≡ []
| Wt_atoms_cons x xs a As :
  Γ ⊢ [x] = a ->
  list_nth x Γ a ->
  wt_atom Γ a ->
  wt_atoms Γ xs As ->
  wt_atoms Γ (x :: xs) (a :: As)
  where "Γ ⊢ vs ≡ As" := (wt_atoms Γ vs As)
.

Notation "Γ ⊢ vs ≡ As" := (wt_atoms Γ vs As) (at level 70).
Notation "Γ ⊢ a" := (wt_atom Γ a) (at level 70).

Inductive wt_atom_list (Γ : env) : list atom -> Prop :=
| Wt_atom_list_nil :
  wt_atom_list Γ []
| Wt_atom_list_cons a As :
  wt_atom Γ a ->
  wt_atom_list (a :: Γ) As ->
  wt_atom_list Γ (a :: As)
.

Notation "Γ ⊧ As" := (wt_atom_list Γ As) (at level 70).

Hint Constructors atom_name atom list_nth wt_atom wt_atoms wt_atom_list.

Lemma ex_wt_atom_list : [] ⊧ [A[]; C[0]; E[1;0]].
 apply Wt_atom_list_cons. auto.
 apply Wt_atom_list_cons. apply Wt_atom; simpl; auto.
 apply Wt_atom_list_cons. apply Wt_atom; simpl; auto.
 apply Wt_atom_list_cons. auto.
