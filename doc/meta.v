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

Fixpoint preinterp_sigma (t : output_type) (iexp : expression -> Type) : Type :=
  match t with
    | DSigma o None f => 
      sigT (fun x => preinterp_sigma (f x) iexp)
    | DSigma o (Some e) f => 
      sigT (fun x => iexp (e x) -> preinterp_sigma (f x) iexp)
    | DUnit =>
      True
  end.

Fixpoint preinterp_pi (t : transformer_type) (iexp : expression -> Type) : Type :=
  match t with
    | DPi o None f => 
      forall (x : interpret_object_type o), preinterp_pi (f x) iexp
    | DPi o (Some e) f => 
      forall (x : interpret_object_type o), 
        iexp (e x) ->
        preinterp_pi (f x) iexp
    | DOutput s =>
      preinterp_sigma s iexp
  end.
 
