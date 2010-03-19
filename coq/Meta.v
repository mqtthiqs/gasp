Require Import Peano_dec.
Require Import List.
Require Import Wf.
Require Import Program.

Definition decidable P := {P}+{~P}.
Definition eq_dec A := forall x y : A, decidable (x=y).
Hint Unfold decidable eq_dec.

Module Type S.
  Parameter atom_name : Type.
  (* Parameter atom_name_eq_dec : eq_dec atom_name. *)
  Parameter transformer : Type.
  (* Parameter transformer_eq_dec : eq_dec transformer. *)
End S.

Module Type F1 (Import X : S).
(* deBuijn representation *)
  Definition var := nat.
  Definition var_eq_dec : eq_dec var := eq_nat_dec.
  
  Inductive atom :=
  | Atom (A : atom_name) (xs : list var).

  Coercion Atom : atom_name >-> Funclass.

  Inductive judgement :=
  | Jdecl (atom : atom)
  | Jassign (atoms : list atom) (T : transformer) (args : list var).
  
  Inductive arity := {
    ar_args : list judgement;
    ar_concls : list judgement
  }.
End F1.

Module Type F2 (Import X : S) (Import Y : F1 X).
  Parameter arity_of_atom : atom_name -> list atom.  
  Parameter arity_of_transformer : transformer -> arity.
End F2.

Module Type F12 (X : S) := F1 X <+ F2 X.

Module F3 (Import X : S) (Import Y : F12 X).
  
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
  
  Definition renaming := list var.
  
  Inductive rename_vars : list var -> renaming -> list var -> Prop :=
  | Rename_nil σ :
    rename_vars [] σ []
  | Rename_cons x xs y ys σ :
    list_nth x σ y ->
    rename_vars xs σ ys ->
    rename_vars (x :: xs) σ (y :: ys)
    .

  (* TODO lifting *)
  
  Inductive wt_assign (Γ : env) : renaming -> 
    list var -> transformer -> list atom -> 
    renaming -> env -> Prop :=

  | Wt_assign σ args concls T Γ' Γ'' σ' σ'':
    wt_args Γ σ args (ar_args (arity_of_transformer T)) σ' Γ' ->
    wt_concls Γ' σ' concls (ar_concls (arity_of_transformer T)) σ'' Γ'' ->
    wt_assign Γ σ args T concls σ'' Γ''

  with wt_args (Γ : env) : renaming -> 
    list var -> list judgement -> 
    renaming -> env -> Prop :=

  | Wt_args_nil σ :
    wt_args Γ σ [] [] σ Γ

  | Wt_args_decl σ x xs ys A zs js σ' Γ':
    rename_vars zs σ ys ->
    list_nth x Γ (A ys) -> 
    wt_args (A ys :: Γ) (x::σ) xs js σ' Γ' ->
    wt_args Γ σ (x::xs) (Jdecl (A zs) :: js) σ' Γ'

  | Wt_args_assign σ xs concls T args js σ' Γ' :
    wt_assign Γ σ args T concls σ' Γ' ->
    wt_args Γ σ xs (Jassign concls T args :: js) σ' Γ'

  with wt_concls (Γ : env) : renaming -> 
    list atom -> list judgement -> 
    renaming -> env -> Prop :=

  | Wt_concls_nil σ :
    wt_concls Γ σ [] [] σ Γ

  | Wt_concls_decl σ A xs ys ats js σ' Γ':
    rename_vars xs σ ys ->      (* ptet l'inverse, ptet autre chose... *)
    wt_concls (A ys :: Γ) (0 :: σ) ats js σ' Γ' -> (* substitution ss doute fausse *)
    wt_concls Γ σ (A xs :: ats) (Jdecl (A ys) :: js) σ' Γ'

  (* TODO assign *)
    .

  Inductive wt_judgements (Γ : env) : list judgement -> Prop :=
  | Wt_judgements_nil :
    wt_judgements Γ []

  | Wt_judgements_cons A js :
    wt_judgements (A :: Γ) js ->
    wt_atom Γ A ->              (* besoin? *)
    wt_judgements Γ (Jdecl A :: js)

  | Wt_judgements_assign concls args js T Γ' σ' :
    wt_assign Γ [] args T concls σ' Γ' ->
    wt_judgements Γ' js ->
    wt_judgements Γ (Jassign concls T args :: js)
    .

  Hint Constructors atom judgement arity list_nth wt_atom wt_atoms
    wt_atom_list rename_vars wt_judgements wt_assign wt_args wt_concls : patch.

End F3.

Module Import Example.
  Inductive an := A | B | C | D | E.
  Definition atom_name := an.
  Inductive t := T | U | V.
  Definition transformer := t.

  Include F1.

  (* Hack pour bénéficier de la coercion, a cause de la redirection an -> atom_name *)
  Definition Atom2 : an -> list var -> atom := Atom.
  Coercion Atom2 : an >-> Funclass.

  Definition arity_of_atom (a : atom_name) : list atom :=
    match a with
      | A => []   | B => []
      | C => [ A []]  | D => [A []; B []]
      | E => [A []; C [1]]
    end.

  Definition arity_of_transformer (t : transformer) := 
    match t with
      | T => 
        {| ar_args := [Jdecl (A []); Jdecl (C [0])]; 
           ar_concls := [Jdecl (B []); Jdecl (D [2;0])] |}
      | U => {| ar_args := []; ar_concls := [] |}
      | V => {| ar_args := []; ar_concls := [] |}
    end.

  Include F3.

  Ltac p := eauto with patch.

  Lemma ex_atom : wt_atom [B []; A []] (D [1;0]).
        apply Wt_atom; eapply Wt_atoms_cons; p.
    (* impossible ac juste eauto?? pourquoi? *)
  Qed.
  
  (* ce lemme *mériterait* une preuve plus compacte :) *)
  Lemma wf_atoms : well_founded
    (fun a b => match b with Atom b _ => In a (arity_of_atom b) end).
    red; intros; induction a; induction A0; intros;
      repeat try (apply Acc_intro; intros; destruct H; subst; try destruct H0).
  Qed.

  Lemma ex_wt_judgement_decls : wt_judgements []
    [ Jdecl (A []);
      Jdecl (B []);
      Jdecl (D [1;0]);
      Jdecl (C [2])
    ].  (* Meme raison, on doit faire a la main *)
    apply Wt_judgements_cons; p. 
    apply Wt_judgements_cons; p.
    apply Wt_judgements_cons; p.
    apply Wt_judgements_cons; p.
    apply Wt_atom; eapply Wt_atoms_cons; p.
    apply Wt_atom; eapply Wt_atoms_cons; p.
  Qed.

  Lemma ex_wt_jdugement_decls2 : wt_judgements []
    [ Jdecl (A []);
      Jdecl (C [0]);
      Jdecl (D [1;0]);
      Jdecl (C [2])
    ].  (* Meme raison, on doit faire a la main *)
    apply Wt_judgements_cons; p.
    apply Wt_judgements_cons; p.
    apply Wt_judgements_cons; p.
    apply Wt_judgements_cons; p.
    apply Wt_atom; eapply Wt_atoms_cons; p.
    apply Wt_atom; eapply Wt_atoms_cons; p.
    eapply Wt_atoms_cons; p. p.
    (* et en plus semble faux a cause des liftings pas faits *)
  Abort.

  (* Marche pas a cause des liftings pas faits *)
  Lemma ex_wt_assign : wt_assign [C [1]; B []; A []]
    [] [2;0] T [B []; D [3;2]] [] 
    [A []; B []; C [1]; B []; D [3;2]].
    eapply Wt_assign; simpl.
    eapply Wt_args_decl. p. p.
    eapply Wt_args_decl. p. p. 
  Abort.

End Example.
