Require Import List.
Require Import Program.
Require Import Peano_dec.

Definition var := nat.

Inductive atom_name := A | B | C | D | E.

Inductive atom :=
| Atom (A : atom_name) (xs : list var).

Coercion Atom : atom_name >-> Funclass.

Definition arity_of_atom (a : atom_name) : list atom :=
  match a with
    | A => []   | B => []
    | C => [ A []]  | D => [A []; B []]
    | E => [A []; C [0]]
  end.

Definition env := list atom.

Inductive list_nth {A} : nat -> list A -> A -> Prop :=
| Nth_o a tl : list_nth 0 (a::tl) a
| Nth_s m a b tl : 
  list_nth m tl a ->
  list_nth (S m) (b :: tl) a
  .

Definition rename_var y x k := 
  match y with
    | 0 => x+k
    | S n => n
  end.

Fixpoint rename_vars xs x k : list var :=
  match xs with
    | [] => []
    | y::xs => rename_var y x k :: rename_vars xs x k
  end.

Fixpoint rename_env As x k : env :=
  match As with
    | [] => []
    | Atom a xs :: As => 
      Atom a (rename_vars xs x k) :: rename_env As x (S k)
  end.

Inductive env_chop : env -> nat -> env -> Prop :=
| Env_chop_o Γ :
  env_chop Γ 0 Γ
| Env_chop_s n a Γ Γ' :
  env_chop Γ n Γ' ->
  env_chop (a :: Γ) (S n) Γ'
.

Inductive wt_atoms (Γ : env) : list var -> list atom -> Prop :=
| Wt_atoms_nil :
  wt_atoms Γ [] []
| Wt_atoms_cons Γ' x xs As (A:atom_name) ys :
  list_nth x Γ (A ys) ->
  env_chop Γ (S x) Γ' ->
  wt_atoms Γ' ys (arity_of_atom A) ->
  wt_atoms Γ xs (rename_env As x 0) ->
  wt_atoms Γ (x :: xs) (A ys :: As)

with wt_atom_list (Γ : env) : list atom -> Prop :=
| Wt_atom_list_nil :
  wt_atom_list Γ []
| Wt_atom_list_cons A xs As :
  wt_atoms Γ xs (arity_of_atom A) ->
  wt_atom_list (A xs :: Γ) (rename_env As (length Γ) 0) ->
  wt_atom_list Γ (A xs :: As)
.

Hint Constructors list_nth env_chop wt_atoms
  wt_atom_list : patch.

Ltac p := simpl;
  try apply Wt_atom_list_cons;
    try eapply Wt_atoms_cons; progress eauto with patch || p.


Lemma ex_wt_atom_list2 : wt_atom_list [B []; A []] [D [1;0]].
  p.
Qed.

Lemma ex_wt_atom_list : wt_atom_list [] [A[]; C[0]; E[1;0]].
  p. 
Qed.
