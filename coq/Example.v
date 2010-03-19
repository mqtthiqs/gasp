Require Import List.
Require Import Peano_dec.
Require Import Program.
Require Import Meta.

Module Import Patch.
  Inductive an := A | B | C | D.
  Definition atom_name := an.
  Definition arity_of_atom a :=
    match a with
      | A => []   | B => []
      | C => [A]  | D => [A;B]
    end.

  Inductive t := T | U | V.
  Definition transformer := t.

  Include F1.

  Definition arity_of_transformer t := 
    match t with
      | T => {| ar_args := []; ar_concls := [] |}
      | U => {| ar_args := []; ar_concls := [] |}
      | V => {| ar_args := []; ar_concls := [] |}
    end.

  Include Make.

End Patch.


