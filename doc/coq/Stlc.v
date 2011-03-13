(* Simply Type Lambda-Calculus, formalized *)

Inductive type : Type :=
| base
| arr (A B : type).

Inductive term : Type :=
| var (x : nat)
| app (t u : term)
| lam (x : nat) (A : type) (t : term).

Inductive env : Type :=
| enil
| econs (x : nat) (A : type) (tl : env).

Inductive lookup (x : nat) (A : type) : env -> Type :=
| look_it {tl} : lookup x A (econs x A tl)
| look_next {y B tl} : lookup x A tl -> lookup x A (econs y B tl).

Inductive of : env -> term -> type -> Type :=
| Var {Γ} x {A} : lookup x A Γ -> of Γ (var x) A
| App {Γ t u} A {B} : of Γ t (arr A B) -> of Γ u A -> of Γ (app t u) B
| Lam {Γ x t A B} : of (econs x A Γ) t B -> of Γ (lam x A t) (arr A B).

Require Import data_meta.
Require Import Peano_dec.

Module V <: VAR.
  Definition var := nat.
  Definition var_eq_dec := eq_nat_dec.
End V.

Import V.

(* We now define atoms and transformers *)

Inductive atom := 
| Atype
| Aterm
| Aenv
| Alookup (x A E : var)
| Aof (E t A : var).

Definition atom_eq_dec : eq_dec atom.
red; intros; destruct x; destruct y;
(left; reflexivity) || (right; discriminate) || idtac.
case (var_eq_dec x x0); case(var_eq_dec A A0); case(var_eq_dec E E0);
intros; subst; auto; right; intro H; injection H; contradiction.
case (var_eq_dec t t0); case(var_eq_dec A A0); case(var_eq_dec E E0);
intros; subst; auto; right; intro H; injection H; contradiction.
Qed.

Inductive transformer :=
| Tbase | Tarr | Tvar | Tapp | Tlam | Tnil | Tcons 
| Tlook_it | Tlook_next | TVar | TApp | TLam.

Lemma transformer_eq_dec : eq_dec transformer.
red; intros. destruct x; destruct y;
(left; reflexivity) || (right; discriminate).
Qed.


Module L <: LANG.
  Definition atom := atom.
  Definition atom_eq_dec := atom_eq_dec.
  Definition transformer := transformer.
  Definition transformer_eq_dec := transformer_eq_dec.
End L.

(* We can apply the functor *)

Module PL := Patch L V.
Import PL.

(* Some example transformer arities. Their creation should be
   automatizable in Ltac. *)

Definition Sbase := pair Tbase 
  {| ar_args := List.nil; 
     ar_concls := Jdecl {| d_var := 1; d_params := nil; d_atom := Atype  |} :: nil
  |}.

Definition Sarr := pair Tarr
  {| ar_args := Jdecl {| d_var := 1; d_params := nil; d_atom := Atype |} :: nil;
     ar_concls := Jdecl {| d_var := 2; d_params := nil; d_atom := Atype |} :: nil
  |}.

Definition Svar := pair Tvar
  {| ar_args := Jdecl {| d_var := 1; d_params := nil; d_atom := Atype |} :: nil;
     ar_concls := Jdecl {| d_var := 2; d_params := nil; d_atom := Atype |} :: nil
  |}.

