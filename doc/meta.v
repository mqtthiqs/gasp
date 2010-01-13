Parameter atom : Type.
Hypothesis atom_dec : forall (x y : atom), {x = y} + {~ x = y}.

Inductive type : Type :=
| Nat : type
| Arrow : type -> type -> type.

Parameter env : Type.
Parameter lookup : env -> atom -> option type.
Parameter bind : atom -> type -> env -> env.

Inductive term : Type :=
| Var : atom -> term
| App : term -> term -> term
| Lam : atom -> type -> term -> term.

Inductive has_ty : env -> term -> type -> Prop :=
| tyVar : forall x ty Γ, 
    lookup Γ x = Some ty 
 -> has_ty Γ (Var x) ty

| tyApp : forall Γ t1 t2 ty1 ty2,
   has_ty Γ t1 (Arrow ty1 ty2)
-> has_ty Γ t2 ty1
-> has_ty Γ (App t1 t2) ty2

| tyLam : forall Γ x ty1 ty2 t,
  has_ty (bind x ty1 Γ) t ty2 ->
  has_ty Γ (Lam x ty1 t) (Arrow ty1 ty2).

(** Il faudrait trouver un type général à donner à une représentation
   syntaxique de ces règles. *)

Record judgment := mkJudgment {
  jenv : env;
  jterm : term;
  jtype : type
}.

Notation "Γ ⊢ e : τ" := (mkJudgment Γ e τ) (at level 30).



Definition lam_spec :=
  forall Γ x t ty1 ty2, 
    mkJudgment ((x, ty1) :: Γ) t ty2 ->
    True.