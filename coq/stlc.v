Inductive type : Type :=
| base
| arr (A B : type).

Inductive term : Type :=
| var (x : nat)
| app (t u : term)
| lam (x : nat) (A : type) (t : term).

Inductive env : Type :=
| nil
| cons (x : nat) (A : type) (tl : env).

Inductive lookup (x : nat) (A : type) : env -> Type :=
| look_it {tl} : lookup x A (cons x A tl)
| look_next {y B tl} : lookup x A tl -> lookup x A (cons y B tl).

Inductive of : env -> term -> type -> Type :=
| Var {Γ} x {A} : lookup x A Γ -> of Γ (var x) A
| App {Γ t u} A {B} : of Γ t (arr A B) -> of Γ u A -> of Γ (app t u) B
| Lam {Γ x t A B} : of (cons x A Γ) t B -> of Γ (lam x A t) (arr A B).

Require Import data_meta.

Inductive atom := 
| Atype
| Aterm
| Aenv
| Alookup (x A E : var)
| Aof (E t A : var).

Inductive transformer :=
| Tbase | Tarr | Tvar | Tapp | Tlam | Tnil | Tcons 
| Tlook_it | Tlook_next | TVar | TApp | TLam.

Module L : LANG.
  Definition atom := atom.
  Definition transformer := transformer.
End L.

Module PL := Patch L.
