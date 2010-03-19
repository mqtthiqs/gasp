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

Inductive list_nth {A} : nat -> list A -> A -> Prop :=
| Nth_o a tl : list_nth 0 (a::tl) a
| Nth_s m a b tl : 
  list_nth m tl a ->
  list_nth (S m) (b :: tl) a
  .

Inductive wt_atom (Γ : env) : atom -> Prop :=
| Wt_atom A xs :
  wt_atoms Γ xs (arity_of_atom A) ->
  wt_atom Γ (A xs)

with wt_atoms (Γ : env) : list var -> list atom -> Prop :=
| Wt_atoms_nil :
  wt_atoms Γ [] []
| Wt_atoms_cons x xs a As :
  list_nth x Γ a ->
  wt_atom Γ a ->
  wt_atoms Γ xs As ->
  wt_atoms Γ (x :: xs) (a :: As)
  .

Inductive wt_atom_list (Γ : env) : list atom -> Prop :=
| Wt_atom_list_nil :
  wt_atom_list Γ []
| Wt_atom_list_cons a As :
  wt_atom Γ a ->
  wt_atom_list (a :: Γ) As ->
  wt_atom_list Γ (a :: As)
.

Hint Constructors atom_name atom list_nth wt_atom wt_atoms wt_atom_list.

Lemma ex_wt_atom_list : wt_atom_list [] [A[]; C[0]; E[1;0]].
 apply Wt_atom_list_cons. auto.
 apply Wt_atom_list_cons. apply Wt_atom; simpl; auto.
 apply Wt_atom_list_cons. apply Wt_atom; simpl; auto.
 apply Wt_atoms_cons; auto.
 apply Wt_atoms_cons; auto.     (* pas possible, bug de dB *)
Abort.
