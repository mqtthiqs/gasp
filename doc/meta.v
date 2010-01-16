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

Inductive signature : Type :=
| Atom : signature
| Typ  : signature
| Env  : signature
| Term : signature 
| HasTy : env -> term -> type -> signature.

Fixpoint interpret_signature (s : signature) : Type :=
  match s with
    | Atom => atom
    | Typ => type
    | Env => env
    | Term => term
    | HasTy e t ty => has_ty e t ty
  end.

Require Import List.

(* FIXME: The following two are fold_left...*)
Definition object : Type := sigT interpret_signature.

Hypothesis object_eqdec : forall o1 o2: object, { o1 = o2 } + {~ o1 = o2 }.

Definition properties := object -> bool.

Require Import String.

Inductive dlist : Type :=
| dnil : forall a:signature, interpret_signature a -> dlist
| dcons : forall (a : signature), (interpret_signature a -> dlist) -> dlist.

Fixpoint interpret_abs (s : dlist) : Type :=
  match s with
    | dnil ty _ => interpret_signature ty
    | dcons ty f => forall x: interpret_signature ty, interpret_abs (f x) 
  end.

Record transformer := mk_transformer {
  name   : string;
  spec  : dlist;
(*  pre    : ?; : precondition ? *)
  implementation : interpret_abs spec
}.

Lemma bind2 : 
forall env x y ty0 ty1 ty2 t,
  has_ty (bind y ty0 (bind x ty1 env)) t ty2 ->
  has_ty env (Lam x ty1 (Lam y ty0 t)) (Arrow ty1 (Arrow ty0 ty2)).
intros; constructor.
constructor. 
auto.
Defined.

Check bind2.

Definition bind2_patch :=
  mk_transformer "bind2"
  (dcons Env 
    (fun env => dcons Atom
      (fun x => dcons Atom
        (fun y => dcons Typ
          (fun ty0 => dcons Typ
            (fun ty1 => dcons Typ
              (fun ty2 => dcons Term
                (fun t => dcons (HasTy (bind y ty0 (bind x ty1 env)) t ty2)
                  (fun H => dnil 
                    (HasTy env (Lam x ty1 (Lam y ty0 t)) (Arrow ty1 (Arrow ty0 ty2)))
                    (bind2 env x y ty0 ty1 ty2 t H))))))))))
  bind2.

Extraction bind2_patch.

Require Import FMaps.
Require Import FMapAVL.

Module Make (Mem : WS).

Definition derivation_name := Mem.E.t.

Inductive expression : Type :=
| DerivationName : signature -> derivation_name -> expression
| Transformer : transformer -> list expression -> expression
| Object : forall s:signature, interpret_signature s -> expression.

Inductive patch : Type :=
| New    : derivation_name -> expression -> patch
| Update : derivation_name -> expression -> patch
| Seq    : patch -> patch -> patch.

Record repo := mk_repo {
  derivations : Mem.t expression;
  context     : Mem.t (list derivation_name)
}.

Hypothesis signature_eqdec : forall s1 s2: signature, { s1 = s2 } + { ~ s1 = s2 }.

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

Program Fixpoint check_dlist (spec : dlist) (tys : list (option object)) : option object :=
  match spec, tys with
    | dnil ty x, nil => Some (existT _ ty x)
    | dcons ty f, (Some (existT xty x)) :: tys => 
      if signature_eqdec ty xty then
        check_dlist (f (▹ x)) tys
      else 
        None
    | _, _ => None
  end.
Next Obligation.
Proof. intuition.  inversion H1. inversion H0. Defined.
Next Obligation.
Proof. intuition. inversion H1. inversion H0. Defined.
Next Obligation.
Proof. intuition. inversion H0. inversion H1. Defined.

(** To remove the artificial nat, define an inductive expressing
   that [derivations r] is a dag. *)
Program Fixpoint 
interpret_expression (n : nat) (r : repo) (e : expression) 
: option (sigT interpret_signature) :=
match n with
| O => None
| S n =>
  match e with
    | Object s x => Some (existT _ s x)
    | Transformer t es =>
      let etys := List.map (interpret_expression n r) es in
        check_dlist (spec t) etys 
    | DerivationName s x => 
      match Mem.find x (derivations r) with
        | None => None
        | Some e =>
          match interpret_expression n r e with
            | None => None
            | Some (existT s' y) =>
              if signature_eqdec s s' then
                Some (existT _ s' y)
                else 
                  None
          end
      end
  end
end.

(** Fin du Coq bien type. *)

(** Les invariants de fonctionnement de l'interprete: 
   - toute expression du repo à une interprétation;
   - le repo forme un DAG ; 
   - à chaque (grand) pas d'exécution, on passe d'un repo bien formé à un autre. *)

Fixpoint play (r : repo) (p : patch) : 
  (** Retourne une liste de sous-problemes dont les solutions sont
     des patchs qui assurent la reconstruction d'un repositoire 
     bien forme. *)
  (** FIXME: Quel type pour ces objets-la? *)

| New    : derivation_name -> expression -> patch
| Update : derivation_name -> expression -> patch
| Seq    : patch -> patch -> patch.

(** A suivre ... *)

