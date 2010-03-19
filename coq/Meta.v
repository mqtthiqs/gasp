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
  Parameter arity_of_atom : atom_name -> list atom_name.
  Parameter transformer : Type.
  (* Parameter transformer_eq_dec : eq_dec transformer. *)
End S.

Module Type F1 (Import X : S).
(* deBuijn representation *)
  Definition var := nat.
  Definition var_eq_dec : eq_dec var := eq_nat_dec.
  
  Inductive atom :=
  | Atom (A : atom_name) (xs : list var).
  
  Inductive judgement :=
  | Jdecl (atom : atom)
  | Jassign (atoms : list atom) (T : transformer) (args : list var).
  
  Inductive arity := {
    ar_args : list judgement;
    ar_concls : list judgement
  }.
End F1.

Module Type F2 (Import X : S) (Import Y : F1 X).
  Parameter arity_of_transformer : transformer -> arity.
End F2.

Module Type F12 (X : S) := F1 X <+ F2 X.

Module Make (Import X : S) (Import Y : F12 X).
  
  Definition env := list atom.
  
  Inductive list_nth {A} : nat -> list A -> A -> Prop :=
  | Nth_o a tl : list_nth 0 (a::tl) a
  | Nth_s m a b tl : 
    list_nth m tl a ->
    list_nth (S m) (b :: tl) a
    .

  Inductive wt_atom (Γ : env) : atom -> Prop :=
  | Wt_atom A xs :
    wt_atom_list Γ xs (arity_of_atom A) ->
    wt_atom Γ (Atom A xs)
    
  with wt_atom_list (Γ : env) : list var -> list atom_name -> Prop :=
  | Wt_atom_list_nil :
    wt_atom_list Γ [] []
  | Wt_atom_list_cons x xs ys A As :
    list_nth x Γ (Atom A ys) ->
    wt_atom Γ (Atom A ys) ->
    wt_atom_list Γ xs As ->
    wt_atom_list Γ (x :: xs) (A :: As)
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
    list_nth x Γ (Atom A ys) -> 
    wt_args (Atom A ys :: Γ) (x::σ) xs js σ' Γ' ->
    wt_args Γ σ (x::xs) (Jdecl (Atom A zs) :: js) σ' Γ'

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
    wt_concls (Atom A ys :: Γ) (0 :: σ) ats js σ' Γ' -> (* substitution ss doute fausse *)
    wt_concls Γ σ (Atom A xs :: ats) (Jdecl (Atom A ys) :: js) σ' Γ'

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

  Hint Constructors atom judgement arity list_nth wt_atom
    wt_atom_list rename_vars wt_judgements wt_assign wt_args wt_concls : patch.

End Make.

Module Import Example.
  Inductive an := A | B | C | D | E.
  Definition atom_name := an.
  Definition arity_of_atom (a : atom_name) :=
    match a with
      | A => []   | B => []
      | C => [A]  | D => [A;B]
      | E => [A;B 0]
    end.

  Inductive t := T | U | V.
  Definition transformer := t.

  Include F1.

  Definition arity_of_transformer (t : transformer) := 
    match t with
      | T => 
        {| ar_args := [Jdecl (Atom A []); Jdecl (Atom C [0])]; 
           ar_concls := [Jdecl (Atom B []); Jdecl (Atom D [2;0])] |}
      | U => {| ar_args := []; ar_concls := [] |}
      | V => {| ar_args := []; ar_concls := [] |}
    end.

  Include Make.

  Ltac p := eauto with patch.

  Lemma ex_atom : wt_atom [Atom B []; Atom A []] (Atom D [1;0]).
        apply Wt_atom; eapply Wt_atom_list_cons; p.
    (* impossible ac juste eauto?? pourquoi? *)
  Qed.
  
  Lemma wf_atoms : well_founded (fun a b => In a (arity_of_atom b)).
    red. intros.
    cut (Acc (fun a b : atom_name => In a (arity_of_atom b)) A).
    cut (Acc (fun a b : atom_name => In a (arity_of_atom b)) B).
    induction a; intros; auto.
    (* C *)
    apply Acc_intro; intros. destruct H1; subst. trivial. destruct H1.
    (* D *)
    apply Acc_intro; intros. destruct H1; subst. trivial.
    destruct H1; subst; trivial. destruct H1.
    (* A *) apply Acc_intro; intros; destruct H; subst.
    (* B *) apply Acc_intro; intros; destruct H; subst.
  Qed.

  Lemma ex_wt_judgement_decls : wt_judgements []
    [ Jdecl (Atom A []);
      Jdecl (Atom B []);
      Jdecl (Atom D [1;0]);
      Jdecl (Atom C [2])
    ].  (* Meme raison, on doit faire a la main *)
    apply Wt_judgements_cons; p. 
    apply Wt_judgements_cons; p.
    apply Wt_judgements_cons; p.
    apply Wt_judgements_cons; p.
    apply Wt_atom; eapply Wt_atom_list_cons; p.
    apply Wt_atom; eapply Wt_atom_list_cons; p.
  Qed.

  Lemma ex_wt_jdugement_decls2 : wt_judgements []
    [ Jdecl (Atom A []);
      Jdecl (Atom C [0]);
      Jdecl (Atom D [1;0]);
      Jdecl (Atom C [2])
    ].  (* Meme raison, on doit faire a la main *)


  Lemma ex_wt_assign : wt_assign [Atom C [1]; Atom B []; Atom A []] 
    [] [2;0] T [Atom B []; Atom D[3;2]] [] 
    [Atom A []; Atom B []; Atom C [1]; Atom B []; Atom D [3;2]].
    eapply Wt_assign; simpl.
    eapply Wt_args_decl. p. p.
    eapply Wt_args_decl. p. p. apply Rename_cons. p. apply Rename_nil.

End Example.
