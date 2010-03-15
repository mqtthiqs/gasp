Require Import List.
Require Import Bool.

Open Scope type_scope.

(* Parameters: patch variables, atoms with arity, transformer names *)
Parameter var : Type.
Parameter var_dec : forall x y : var, {x=y}+{x<>y}.

Parameter atom : Type.
Parameter atom_arity : atom -> nat.
Parameter transformer : Type.

(* An atom always comes with its variables (maybe this should be
dependent, we'll see) *)
(* Definition atom := atom_name * list var. *)

(* A declaration is a judgement of type (a : A xs) *)
Record decl := {
  d_var : var;
  d_params : list var;
  d_atom : atom
}.

(* An assignation is a judgement of type ((x:A)s = T(xs)) *)
Record assign := {
  as_decls : list decl;
  as_trans : transformer;
  as_args : list var
}.


(* 
 * Renamings (subsitutions from var to var) 
 *)

Definition renaming := list (var * var).

Fixpoint renaming_assoc a ren : option var :=
  match ren with 
    | nil => None
    | cons (x,y) tl => 
      if var_dec x a then Some y else renaming_assoc a tl
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

Inductive environment := 
| Enil
| Edecl (d : decl) (tl : environment)
| Eassign (a : assign) (tl : environment)
.

Inductive list_forall {A} P : list A -> Prop :=
| Forall_nil : list_forall P nil
| Forall_cons a tl : P a -> list_forall P tl -> list_forall P (a::tl)
.

Inductive in_decl : 
  environment -> decl -> Prop :=
| In_decl_decl_O E d :
  in_decl (Edecl d E) d

| In_decl_assign_O E a d :
  List.In d a.(as_decls) ->
  in_decl (Eassign a E) d

| In_decl_decl_S E d d' :
  in_decl E d ->
  d <> d' ->
  in_decl (Edecl d' E) d

| In_decl_assign_S E d a :
  in_decl E d ->
  list_forall (fun d' => d <> d') a.(as_decls) ->
  in_decl (Eassign a E) d
  .

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

Parameter assign_eq_dec : assign -> assign -> bool.
Parameter assign_eq_dec_adeq1 : 
  forall a b, assign_eq_dec a b = true <-> a = b.
Parameter assign_eq_dec_adeq2 : 
  forall a b, assign_eq_dec a b = false <-> a <> b.

Fixpoint in_assign_dec (E : environment) (a : assign) : bool :=
  match E with
    | Eassign a' E => 
      if assign_eq_dec a a' then true 
        else in_assign_dec E a
    | Edecl d E => in_assign_dec E a
    | Enil => false
  end.

Require Setoid.

Lemma in_assign_dec_adeq E a : Is_true (in_assign_dec E a) <-> in_assign E a.
split.
(* -> *)
induction E; simpl; intro. 
destruct H.
apply In_assign_decl_S. auto.
case_eq (assign_eq_dec a a0); intro.
info rewrite assign_eq_dec_adeq1 in H0. subst. apply In_assign_O.
apply In_assign_assign_S. rewrite H0 in H. auto. 
rewrite assign_eq_dec_adeq2 in H0. trivial.
(* <- *)
intro; induction E; simpl in *.
inversion H.
inversion H. subst. auto.
case_eq (assign_eq_dec a a0); intro. 
simpl; trivial.
apply IHE. inversion H; subst. 
rewrite assign_eq_dec_adeq2 in H0. destruct H0; trivial.
trivial.
Qed.

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
  wt_judgements E nil
  .

Record wt_arity a : Prop := {
  wt_ar_args : wt_judgements Enil a.(ar_args);
  wt_ar_concls : wt_judgements Enil a.(ar_concls)
}.

Inductive wt_application E r :
  list judgement -> list var -> renaming -> Prop :=

| Wt_app_nil :
  wt_application E r nil nil r

| Wt_app_decl r' d js x xs :
  in_decl E (rename_decl r d) ->
  wt_application E ((d.(d_var), x) :: r) js xs r' ->
  wt_application E r (Jdecl d :: js) (x :: xs) r'

| Wt_app_assign r' a js xs :
  in_assign E (rename_assign r a) ->
  wt_application E r (List.map Jdecl a.(as_decls)) xs r' ->
  wt_application E r (Jassign a :: js) xs r'
.

Inductive wt_results E r :
  list decl -> list judgement -> environment -> Prop :=

| Wt_res_nil :
  wt_results E r nil nil E

| Wt_res_decl d ds d' js E':
  d.(d_atom) = d'.(d_atom) ->
  d.(d_params) = rename_list r d'.(d_params) ->
  wt_results (Edecl d E) ((d'.(d_var),d.(d_var))::r) ds js E' ->
  wt_results E r (d::ds) (Jdecl d'::js) E'

(* TODO assign *)
.

Inductive wt_patch (S:signature) E : patch -> Prop :=

| Wt_patch_nil :
  wt_patch S E nil

| Wt_patch_cons a ar p r' E' :
  List.In (a.(as_trans), ar) S ->
  wt_application E nil ar.(ar_args) a.(as_args) r' ->
  wt_results E r' a.(as_decls)  ar.(ar_concls) E' ->
  wt_patch S E' p ->
  wt_patch S E (a :: p)
.