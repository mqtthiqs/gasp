Require Import List Bool Setoid SetoidDec Program Sumbool.

(* Some facts on decidability *)

Definition decidable P := {P}+{~P}.
Definition decidable_b P := {b:bool | if b then P else ~P}.
Definition sumbool_of_sigbool A (b : decidable_b A) : decidable A :=
  match b with
    | exist true H => left H
    | exist false H => right H
  end.
Definition sigbool_of_sumbool A (s : decidable A) : decidable_b A :=
  match s with
    | left H => exist (fun b : bool => if b then A else ~A) true H
    | right H => exist _ false H
  end.

Coercion sumbool_of_sigbool : decidable_b >-> decidable.
Coercion sigbool_of_sumbool : decidable >-> decidable_b.

Definition decidable1 {A} P :=  { x : option A | match x with
                                                | Some a => P a
                                                | None => forall a, ~ P a end }.


Ltac notinv := let H := fresh "H" in intro H; inversion H; subst.

Definition eq_dec A := forall x y : A, decidable (x=y).
Hint Unfold decidable eq_dec.

Program Fixpoint list_assoc {A B} (A_eq_dec : eq_dec A) a (l : list (A*B)) : decidable1 (fun b => In (a,b) l) :=
  match l with 
    | [] => None
    | (k,v) :: tl => if A_eq_dec a k then Some v 
      else list_assoc A_eq_dec a tl
  end.
Next Obligation. induction x; auto. intro. intro. destruct H0. 
  inversion H0. symmetry in H2. contradiction. firstorder.
Defined.

(* Parameters: patch variables, atoms & transformer names *)

Module Type VAR.
  Parameter var : Type.
  Parameter var_eq_dec : eq_dec var.
End VAR.

Module Type LANG.
  Parameter atom : Type.
  Parameter atom_eq_dec : eq_dec atom.
  Parameter transformer : Type.
  Parameter transformer_eq_dec : eq_dec transformer.
End LANG.

Module Patch (Import L : LANG) (Import V : VAR).

(* A declaration is a judgement of type (a : A xs) *)
Record decl := {
  d_var : var;
  d_params : list var;
  d_atom : atom
}.

Definition decl_eq_dec : eq_dec decl.
red; intros. induction x; induction y.
case (var_eq_dec d_var0 d_var1);
case (list_eq_dec var_eq_dec d_params0 d_params1);
case (atom_eq_dec d_atom0 d_atom1); intros; subst;
  auto; right; injection; contradiction.
Defined.

(* An assignation is a judgement of type ((x:A)s = T(xs)) *)
Record assign := {
  as_decls : list decl;
  as_trans : transformer;
  as_args : list var
}.

Definition assign_eq_dec : eq_dec assign.
red; intros; destruct x; destruct y.
case (list_eq_dec decl_eq_dec as_decls0 as_decls1);
case (transformer_eq_dec as_trans0 as_trans1);
case (list_eq_dec var_eq_dec as_args0 as_args1);
  intros; subst; auto; right; injection; contradiction.
Qed.


(* 
 * Renamings (subsitutions from var to var) 
 *)

Definition renaming := list (var * var).

Fixpoint renaming_assoc a ren : option var :=
  match ren with 
    | [] => None
    | (x,y) :: tl => 
      if var_eq_dec x a then Some y else renaming_assoc a tl
  end.

Definition rename_list ren (vl : list var) : list var :=
  List.map 
  (fun a => match renaming_assoc a ren with
              | Some x => x
              | None => a
            end
  ) vl.

Definition rename_decl ren d :=
  let new_params := rename_list ren d.(d_params) in
    {| d_var   := d.(d_var);
      d_params := new_params;
      d_atom   := d.(d_atom)
    |}.

Definition rename_assign ren a :=
  {| as_decls := List.map (rename_decl ren) a.(as_decls);
    as_trans := a.(as_trans);
    as_args  := rename_list ren a.(as_args)
    |}.

(* A judgement is either a declaration or an assignment *)
Inductive judgement :=
| Jdecl (d : decl)
| Jassign (a : assign).

(* The arity associated to a transformer *)
Record arity := {
  ar_args : list judgement;
  ar_concls : list judgement
}.

(* A signature is a list of transformers with their arity *)
Definition signature := list (transformer * arity).

(* A patch is a list of assignations *)
Definition patch := list assign.


(* 
 * Typing patches:
 *)

(* An environment is a simple list of either a declaration or an
assignment *)

Inductive environment := 
| Enil
| Edecl (d : decl) (tl : environment)
| Eassign (a : assign) (tl : environment)
.

(* The In predicate, for declarations and assignments *)
Inductive in_decl : 
  environment -> decl -> Prop :=
| In_decl_decl_O E d :
  in_decl (Edecl d E) d

| In_decl_assign_O E a d :
  In d a.(as_decls) ->
  in_decl (Eassign a E) d

| In_decl_decl_S E d d' :
  in_decl E d ->
  d <> d' ->
  in_decl (Edecl d' E) d

| In_decl_assign_S E d a :
  in_decl E d ->
  ~ In d a.(as_decls) ->
  in_decl (Eassign a E) d
  .

Hint Constructors in_decl : patch.

Program Fixpoint in_decl_dec' E d : decidable_b (in_decl E d) :=
  match E with
    | Eassign a E => 
      match In_dec decl_eq_dec d a.(as_decls) with
        | left H => true
        | right H => in_decl_dec' E d
      end
    | Edecl d' E =>
      match decl_eq_dec d d' with
        | left H => true
        | right H => in_decl_dec' E d
      end
    | Enil => false
  end.
Next Obligation. auto with patch. Defined.
Next Obligation.
induction x; auto with patch. 
intro K; inversion K; subst; contradiction. Defined.
Next Obligation. auto with patch. Defined.
Next Obligation.
induction x; auto with patch.
intro; apply y. admit. Defined.
Next Obligation. intro H; inversion H; auto. Defined.

Definition in_decl_dec E d : decidable (in_decl E d) := in_decl_dec' E d.

Inductive in_assign :
  environment -> assign -> Prop :=
| In_assign_O E a :
  in_assign (Eassign a E) a

| In_assign_assign_S E a a' :
  in_assign E a ->
  a <> a' ->
  in_assign (Eassign a' E) a

| In_assign_decl_S E a d :
  in_assign E a ->
  in_assign (Edecl d E) a
  .

Fixpoint in_assign_dec E a : decidable (in_assign E a).
intros; refine (
  match E with
    | Eassign a' E => 
      match assign_eq_dec a a' with
        | left H => _
        | right H => _ (in_assign_dec E a)
      end
    | Edecl d E => _ (in_assign_dec E a)
    | Enil => right (fun H => _)
  end); intros.
inversion H.
destruct x; [left | right]. apply In_assign_decl_S; trivial. 
intro H; inversion H; contradiction.
subst. left. apply In_assign_O.
destruct x. left; apply In_assign_assign_S; trivial.
right. intro. inversion H0. symmetry in H1. contradiction. contradiction.
Defined.

(* This isn't used... *)
Inductive wt_judgements E : 
  list judgement -> Prop :=
| Wt_decl d js :
  wt_judgements (Edecl d E) js ->
  wt_judgements E (Jdecl d :: js)

| Wt_assign a js :
  wt_judgements (Eassign a E) js ->
  in_assign E a ->
  wt_judgements E (Jassign a :: js)

| Wt_nil :
  wt_judgements E []
  .

(* The arity (of a transformer) is its "type", a pair of lists of
well-typed judgements *)

Record wt_arity a : Prop := {
  wt_ar_args : wt_judgements Enil a.(ar_args);
  wt_ar_concls : wt_judgements Enil a.(ar_concls)
}.

(* Well-typing of the application of a transformer given the left-hand
side of its arity *)

Inductive wt_application E r :
  list judgement -> list var -> renaming -> Prop :=

| Wt_app_nil :
  wt_application E r [] [] r

| Wt_app_decl r' d js x xs :
  in_decl E (rename_decl r d) ->
  wt_application E ((d.(d_var), x) :: r) js xs r' ->
  wt_application E r (Jdecl d :: js) (x :: xs) r'

| Wt_app_assign r' a js xs :
  in_assign E (rename_assign r a) ->
  wt_application E r (List.map Jdecl a.(as_decls)) xs r' ->
  wt_application E r (Jassign a :: js) xs r'
.

Hint Constructors wt_application in_assign in_decl : patch.
Hint Extern 2 (~ _) => notinv : patch.

Fixpoint wt_application_decls_dec E r ds xs : option renaming :=
  match ds, xs with
    | [], [] => Some r
    | d :: ds, x :: xs =>
      match in_decl_dec E (rename_decl r d) with
        | left _ => wt_application_decls_dec E ((d.(d_var), x) :: r) ds xs
        | right _ => None
      end 
    | _, _ => None
  end.

Program Fixpoint wt_application_dec E r js xs {struct js} 
  : decidable1 (fun r' => wt_application E r js xs r') :=
  match js, xs with
    | [], [] => Some r
    | Jdecl d :: js, x :: xs => 
      match in_decl_dec E (rename_decl r d) with
        | left _ => wt_application_dec E ((d.(d_var), x) :: r) js xs
        | right _ => None
      end 
    | Jassign a :: js, _ => 
      match in_assign_dec E (rename_assign r a) with
        | left _ => wt_application_decls_dec E r a.(as_decls) xs
        | right _ => None
      end
    | _, _ => None
  end.
Next Obligation. auto with patch. Defined.
Next Obligation. induction x0; auto with patch.
  intro; notinv. apply (y a); trivial. Defined.
Next Obligation. auto with patch. Defined.
Next Obligation. case (wt_application_decls_dec E r (as_decls a) xs).
admit. admit. Defined.
Next Obligation. auto with patch. Defined.
Next Obligation. admit. Defined.
Next Obligation. auto with patch. admit. Defined.
Next Obligation. admit. Defined.

(* Well-typing of the resulting assignation *)

Inductive wt_results E r :
  list decl -> list judgement -> environment -> Prop :=

| Wt_res_nil :
  wt_results E r [] [] E

| Wt_res_decl d ds d' js E':
  d.(d_atom) = d'.(d_atom) ->
  d.(d_params) = rename_list r d'.(d_params) ->
  wt_results (Edecl d E) ((d'.(d_var),d.(d_var))::r) ds js E' ->
  wt_results E r (d::ds) (Jdecl d'::js) E'

(* TODO assign *)
.

Hint Constructors wt_results : patch.

Program Fixpoint wt_results_dec E r ds js : 
  { x : option environment | match x with
                            | Some E' => wt_results E r ds js E'
                            | None => forall E', ~ wt_results E r ds js E' end } :=
  match ds, js with 
    | [], [] => Some E
    | d :: ds, Jdecl d' :: js => 
      if sumbool_and _ _ _ _ (atom_eq_dec d.(d_atom) d'.(d_atom)) 
        (list_eq_dec var_eq_dec d.(d_params) (rename_list r d'.(d_params))) 
        then wt_results_dec (Edecl d E) ((d'.(d_var),d.(d_var))::r) ds js 
        else None
    | _, _ => None
  end.
Next Obligation. auto with patch. Defined.
Next Obligation. induction x. auto with patch. intro; notinv. 
  inversion H1; subst. firstorder. Defined.
Next Obligation. notinv; tauto. Defined.
Next Obligation. notinv; auto with patch. admit. Defined.
Next Obligation. admit. Defined.
Next Obligation. admit. Defined.
Next Obligation. admit. Defined.

(* Well-typing of patches *)

Inductive wt_patch (S:signature) E : patch -> Prop :=

| Wt_patch_nil :
  wt_patch S E []

| Wt_patch_cons a ar p r' E' :
  List.In (a.(as_trans), ar) S ->
  wt_application E [] ar.(ar_args) a.(as_args) r' ->
  wt_results E r' a.(as_decls) ar.(ar_concls) E' ->
  wt_patch S E' p ->
  wt_patch S E (a :: p)
.

Hint Constructors wt_patch : patch.

Program Fixpoint wt_patch_dec S E p : decidable (wt_patch S E p) :=
  match p with
    | [] => left _
    | a :: p => 
      match list_assoc transformer_eq_dec a.(as_trans) S with
        | None => right _
        | Some ar =>
          match wt_application_dec E [] ar.(ar_args) a.(as_args) with
            | None => right _
            | Some r' =>
              match wt_results_dec E r' a.(as_decls) ar.(ar_concls) with
                | None => right _
                | Some E' => left _
              end
          end
      end
  end.
Next Obligation. auto with patch. Defined.
Next Obligation. admit. Defined.
Next Obligation. admit. Defined.
Next Obligation. admit. Defined.
Next Obligation. admit. Defined.

End Patch.
