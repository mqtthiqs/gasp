 (** Some piece of Coq to illustrate notes of
   Sat Jan 16, 2010  7:21 PM 
 *)

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

Inductive has_ty : env -> term -> type -> Type :=
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

Inductive object_type : Type :=
| Atom : object_type
| Typ  : object_type
| Env  : object_type
| Term : object_type 
| HasTy : env -> term -> type -> object_type.

Fixpoint interpret_object_type (s : object_type) : Type :=
  match s with
    | Atom => atom
    | Typ => type
    | Env => env
    | Term => term
    | HasTy e t ty => has_ty e t ty
  end.


Require Import Program.

(** [coerce T U Hid x] is an explicit coercion from [x] of type [T] to
   a value [y] of type [U], which is justified by a type equality
   [Hid]. [x] and [y] are related by a John Major equality. *)
Lemma coerce : forall (T U : Type), T = U -> forall (x : T),
  { y : U | JMeq x y }.
Proof.
  intros. subst. exists x. constructor.
Defined.

(** Most of the time, the type inference engine of Coq can help us to
   determine [T] and [U]. *)
Notation "▹ t" := (coerce _ _ _ t) (at level 10).

(** When [T = U], this tactic turns a dependent equality 
   into a Leibniz' and substitute it right away. *)
Ltac substT H :=
  generalize (inj_pair2 _ _ _ _ _ H); intro; subst; clear H.

(** When [T = U], a coercion is the identity. *)
Lemma coerce_id : forall T (x : T) H, proj1_sig (coerce T T H x) = x.
Proof.
  intros.
  destruct (coerce T T H x). simpl. apply JMeq_eq; auto.
Qed. 

Require Import FMaps.
Require Import FMapAVL.

Module Make (Mem : WS).

Definition derivation_name := Mem.E.t.
Definition transformer_name := Mem.E.t.

Inductive output_type := 
| DSigma  : forall (o : object_type), 
  option (interpret_object_type o -> expression) ->
  (interpret_object_type o -> output_type) 
  -> output_type 
| DUnit : output_type 

with transformer_type := 
| DPi  : forall (o : object_type), 
  option (interpret_object_type o -> expression) ->
  (interpret_object_type o -> transformer_type) 
  -> transformer_type
| DOutput : output_type -> transformer_type

with expression : Type :=
| DerivationName : 
  derivation_name -> object_type -> expression
| Transformer : 
  transformer_name -> transformer_type
  -> list (derivation_name * object_type)
  -> expression. 

Definition object : Type := sigT interpret_object_type.

Definition object_env := Mem.t object.

Definition bind_object (oenv : object_env) x xty v :=
  Mem.add x (existT _ xty v) oenv.

Definition derivation_has_type x ty (env : object_env) :=
  exists o, Mem.MapsTo x (existT _ ty o) env.

Program Definition derivation_lookup x ty env (H : derivation_has_type x ty env) 
  : interpret_object_type ty :=
  match Mem.find x env with
    | None => !
    | Some (existT ty' o) => ▹ o
  end.
Next Obligation.
  unfold derivation_has_type in H. destruct H.
  generalize (Mem.find_1 (elt := object) H).
  congruence.
Qed.
Next Obligation.
  unfold derivation_has_type in H. 
  destruct H.
  generalize (Mem.find_1 (elt := object) H).
  intro Hin. rewrite Hin in *.
  inversion Heq_anonymous.
  subst. 
  simpl.
  auto.
Qed.

Definition transformer_object := sigT (fun (T : Type) => T).

Definition transformer_env := Mem.t transformer_object.

Inductive interpret_transformer_type : 
  object_env ->
  transformer_env ->
  transformer_type ->
  Type ->
  Type
:=
| interpret_pi_type : 
  forall oenv tenv oty f F,
    interpret_transformer_type_functional oenv tenv oty f F ->
    interpret_transformer_type oenv tenv 
    (DPi oty None f)
    (forall xc, F xc)

| interpret_pidef_type : 
  forall oenv tenv oty e E f F,
    interpret_transformer_type_functional oenv tenv oty f F ->
    interpret_expression oenv tenv oty e E ->
    interpret_transformer_type oenv tenv 
    (DPi oty None f)
    (forall xc, xc = E -> F xc)

with interpret_transformer_type_functional :
  object_env ->
  transformer_env ->
  forall (o : object_type),
    (interpret_object_type o -> transformer_type) ->
    (interpret_object_type o -> Type) ->
    Type :=
| wf_interpret_transformer_type_functional:
  forall oenv tenv oty (x : interpret_object_type oty) f F, 
    interpret_transformer_type oenv tenv (f x) (F x) ->
    interpret_transformer_type_functional oenv tenv oty f F

with interpret_expression :
  object_env ->
  transformer_env ->
  forall (o : object_type),
    expression ->
    interpret_object_type o ->
    Type :=

| interpret_derivation_name:
  forall oenv tenv oty d,
    forall (H : derivation_has_type d oty oenv), 
    interpret_expression oenv tenv oty (DerivationName d oty) (derivation_lookup d oty oenv H).

(** Difficulte: comment exprimer que l'application est bien typé. *)
| interpret_transformer_app:
  forall oenv tenv t tty ys, 
    forall T fo,
      MapsTo t (existT T fo) tenv ->
      interpret_transformer_type oenv tenv tty fo ->
    interpret_expression oenv tenv oty (Transformer t tty ys) ...
