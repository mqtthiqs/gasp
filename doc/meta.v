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

Definition object : Type := sigT interpret_object_type.

Require Import String.

Inductive spec :=
| DUnit   : spec
| DPi     : forall (o : object_type), 
  (interpret_object_type o -> spec) -> spec
| DSigma  : forall (o : object_type), 
  (interpret_object_type o -> spec) -> spec.

Fixpoint interpret_spec (s : spec) : Type :=
  match s with
    | DUnit => True
    | DPi ty f => forall (x : interpret_object_type ty), interpret_spec (f x)
    | DSigma ty f => sigT (fun (x : interpret_object_type ty) => interpret_spec (f x))
  end.

Record transformer := mk_transformer {
  tname          : string;
  tspec          : spec;
  timplementation : interpret_spec tspec
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

(* I thought that Quote would have been nice to transform
   automatically our Coq type into a spec. Yet, the quote tactic does
   not seem to deal with "forall x. ...". We shall use LTac instead.*)
Require Import Quote.

Program Definition bind2_patch :=
  mk_transformer "bind2"
  (DPi Env 
    (fun env => DPi Atom
      (fun x => DPi Atom
        (fun y => DPi Typ
          (fun ty0 => DPi Typ
            (fun ty1 => DPi Typ
              (fun ty2 => DPi Term
                (fun t => DPi (HasTy (bind y ty0 (bind x ty1 env)) t ty2)
                  (fun H => DSigma 
                    (HasTy env (Lam x ty1 (Lam y ty0 t)) (Arrow ty1 (Arrow ty0 ty2)))
                    (fun _ => DUnit))))))))))
  _.
Next Obligation. eexists; [apply bind2|]; auto.
Defined.

Require Import FMaps.
Require Import FMapAVL.

Module Make (Mem : WS).

Definition derivation_name := Mem.E.t.

Inductive expression : Type :=
| DerivationName : object_type -> derivation_name -> expression
| Join : list expression -> expression
| Transformer : transformer -> expression -> expression.

Record repo := mk_repo {
  derivations : Mem.t expression
}.

Hypothesis object_type_eqdec : forall s1 s2: object_type, { s1 = s2 } + { ~ s1 = s2 }.

Program Fixpoint apply_transformer 
  (spec : spec) (t : interpret_spec spec) (args : list object)
: option (list object) :=
  match spec, args with
    | DUnit, nil => Some nil
    | DPi ty f, (existT xty x) :: os => 
      let t' := ((▹ t) : forall (o: interpret_object_type ty), interpret_spec (f o)) in
      if object_type_eqdec ty xty then
        let x' := ((▹ x) : interpret_object_type ty) in
        apply_transformer (f x') (t' x') os
      else 
        None
    | DSigma ty f, nil => 
      let t' := ((▹ t) : sigT (fun (x : interpret_object_type ty) => interpret_spec (f x))) in
      let o  := projT1 t' in
        match apply_transformer (f o) (projT2 t') nil with
          | None => None
          | Some os => Some (existT _ ty o :: os)
        end
    | _, _ => None
  end.
Solve Obligations using (program_simpl; intuition; congruence; auto).

(** To remove the artificial nat, define an inductive expressing
   that [derivations r] is a dag: 
   
   Proposal (maybe too naive but very simple):

   1. Attach a function "depth_var : var -> nat" to every repository.
   2. Define "depth : repo -> exp -> nat" by induction:
      x               => depth_var repo x
    | tuple ts        => 1 + sum depth ts
    | transformer f e => 1 + depth e
    3. Define "wf_repo : repo -> Prop := forall (x |-> e) in repo, depth_var x > depth e

*)

(** FIXME: We could use the Error monad here. *)
(** Use depth as a measure. *)
Program Fixpoint 
interpret_expression (n : nat) (r : repo) (e : expression) : option (list object) :=
match n with
| O => None
| S n =>
  match e with
    | Transformer t e =>
      match interpret_expression n r e with
        | None => None
        | Some args => apply_transformer (tspec t) (timplementation t) args
      end
    | Join es =>
      List.fold_right (fun accu x =>
         match accu with None => None
           | Some l => match x with Some [x] => Some (x :: l)
                         | _ => None 
                       end
         end) (Some []) (List.map (interpret_expression n r) es)
    | DerivationName s x => 
      match Mem.find x (derivations r) with
        | None => None
        | Some e =>
          match interpret_expression n r e with
            | Some [existT s' y] =>
              if object_type_eqdec s s' then
                  Some [existT _ s' y]
                else 
                  None
            | _ => None
          end
      end
  end
end.

(** Claim: Si le programme est bien typé et que le repo est WF alors on obtient une 
   valeur du type attendu par cette fonction interprétation. *)

(** Ensuite: écrire les fonctions de rajout d'une expression dans le repo, en 
   maintenant la bonne formation. Ca devrait aller. *)

(** Puis: considérer des opérations plus méta comme par exemple la jointure de 
   deux repos. A quelles conditions peut-on maintenir la bonne formation? 
   Quels méta-théorèmes sur le langage de patchs peut-on en déduire? *)

(** Enfin: essayer d'enrichir ces fonctions avec l'information supplémentaire: qui utilise
   quelle dérivation (le champ [context] dans la version précédente de ce fichier).

   On doit alors pouvoir exhiber des propriétés intéressantes sur les jointures.
*)

(** En conclusion: auto-appliquer! *)