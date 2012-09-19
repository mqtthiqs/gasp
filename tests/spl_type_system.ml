#use "load.ml"
;;

let repo = Version.init
<:sign<

(* % *)
(* % SYNTAX *)
(* % *)
program     : type.
declaration : type.
binding     : type.
expression  : type.
typ         : type.
char	    : type.
identifier  : type.
int	    : type.

(* %% Program *)
(* %%--------- *)
declprog  : declaration -> program.
joinprog  : program -> program -> program.
emptyprog : program.

(* %% Declaration *)
(* %%------------- *)
valdecl : binding -> expression -> declaration.

(* %% Binding *)
(* %%--------- *)
bind : identifier -> typ -> binding.

(* %% Expression *)
(* %%------------ *)
var  : identifier -> expression.
eint : int -> expression.

(* %% Type *)
(* %%------ *)
tint : typ.

(* %% Literals *)
(* %%--------- *)
quarante_deux : int.

(* %% Identifier *)
(* %%----------- *)
a : char.
b : char.
minus  : char -> identifier -> identifier.
minus_ : identifier.

(* % *)
(* % Type system *)
(* % *)

tenv : type.
tc_expression  : tenv -> expression -> typ -> type.
tc_program     : tenv -> program -> tenv -> type.
tc_declaration : tenv -> declaration -> tenv -> type.

(* %% Typing environment *)
(* %%------------------- *)
emptyenv : tenv.
joinenv  : tenv -> tenv -> tenv.
bindval  : binding -> tenv -> tenv.

(* %% Judgments over typing environments *)
(* %%----------------------------------- *)
subenv : tenv -> tenv -> type.
subenv_evidence : {E1 : tenv } { E2 : tenv } subenv E1 E2.

(* %% Equivalence modulo associativity *)
(* %%--------------------------------- *)
equiv : tenv -> program -> tenv -> program -> type.
equiv_evidence :
  { E1 : tenv } { P1 : program } { E2 : tenv } { P2 : program }
  equiv E1 P1 E2 P2.

(* %% Typing rules *)
(* %%------------- *)
tc_declprog :
  { E1 : tenv } { d : declaration } { E2 : tenv } { H : tc_declaration E1 d E2 }
  tc_program E1 (declprog d) E2.

tc_emptyprog :
  { E : tenv } 
  tc_program E emptyprog E.

tc_joinprog :
  { E : tenv } { E1 : tenv } { E' : tenv } { E2 : tenv }
  { P1 : program } { P2 : program }
  { H1 : tc_program E P1 E1 }
  { H2 : subenv E' (joinenv E E1) }
  { H3 : tc_program E' P2 E2 }
  tc_program E (joinprog P1 P2) (joinenv E1 E2).

tc_valdecl :
  { E : tenv } { x : identifier } { ty : typ } { e : expression } 
  { H : tc_expression E e ty }
  tc_declaration E (valdecl (bind x ty) e) (bindval (bind x ty) E).

tc_eint :
  { E : tenv } { x : int } 
  tc_expression E (eint x) tint.

>>
;;
