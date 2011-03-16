open AST
open SLF
open Misc

let ( ! ) x = Position.unknown_pos x

let ( $ ) x y = ! (App (x, ! y))

let ( $$ ) x y = ! (App (x, y))

let atom x = 
  let cons x y = ! (Var "_") $$ x $$ y in
  List.fold_left 
    (fun accu c -> cons (! (Var (ExtString.from_char c))) accu)
    (! (Var "_*"))
    (ExtString.to_list x)

let ilit x	      = Var "42"

let bind_ctor         = Var "bind"

let dvar_ctor         = Var "dvar"

let tint_ctor         = Var "tint"

let eint_ctor         = Var "eint"

let bindval_ctor      = Var "bindval"
let emptyenv_ctor     = Var "emptyenv"
let joinenv_ctor      = Var "joinenv"

let emptyprog_ctor    = Var "emptyprog"
let joinprog_ctor     = Var "joinprog"
let declprog_ctor     = Var "declprog"

let valdecl_ctor      = Var "valdecl"

let subenv_evidence_ctor = Var "subenv_evidence"

let tc_declprog_ctor  = Var "tc_declprog"
let tc_joinprog_ctor  = Var "tc_joinprog"
let tc_emptyprog_ctor = Var "tc_emptyprog"

let tc_valdecl_ctor   = Var "tc_valdecl"

let tc_eint_ctor      = Var "tc_eint"

let identifier : identifier -> SLF.term =
  fun (Identifier x) -> atom x

let typ : typ -> SLF.term = function 
  | TInt -> ! tint_ctor
  | _ -> assert false

let expression : expression -> SLF.term = function
  | EInt x -> ! eint_ctor $ ilit x
  | _ -> assert false

let binding : binding -> SLF.term = 
  fun (id, ty) -> ! bind_ctor $$ identifier id $$ typ ty
  
let rec program : program -> SLF.term = function
  | [] -> ! emptyprog_ctor
  | d :: ds -> ! joinprog_ctor $$ (! declprog_ctor $$ declaration d) $$ program ds
  
and declaration : declaration -> SLF.term = function
  | DVal (b, e) -> ! valdecl_ctor $$ binding b $$ expression e
  | _ -> assert false

let rec tenv : tenv -> SLF.term = function
  | BindVal b        -> ! bindval_ctor $$ binding b
  | EmptyEnv         -> ! emptyenv_ctor
  | JoinEnv (e1, e2) -> ! joinenv_ctor $$ tenv e1 $$ tenv e2
  | _ -> assert false

let subenv_evidence : tenv -> tenv -> SLF.term =
  fun e e' -> 
    ! subenv_evidence_ctor $$ tenv e $$ tenv e' 

(* 
   Γ ⊢_d d ⇒ Γ Γ'
   —————————————– [tc_declprog]
   Γ ⊢_p d ⇒ Γ Γ' 
*)
let tc_declprog : tenv -> declaration -> tenv -> SLF.term -> SLF.term = 
  fun e d e' h -> 
    ! tc_declprog_ctor $$ tenv e $$ declaration d $$ tenv e' $$ h

(* 
   —————————————– [tc_emptyprog]
   Γ ⊢_p • ⇒ Γ
*)
let tc_emptyprog : tenv -> SLF.term = 
  fun e -> 
    ! tc_emptyprog_ctor $$ tenv e

(* 
   Γ ⊢_p P1 ⇒ Γ Γ1   Γ' ⊆ Γ Γ1    Γ' ⊢_p P2 ⇒ (Γ Γ1) Γ2
   —————————————–——————————————–—————————————————————–— [tc_joinprog]
            Γ ⊢_p P1 P2 ⇒ Γ (Γ1 Γ2)
*)
let tc_joinprog : 
    tenv -> tenv -> tenv -> tenv -> program -> program 
  -> SLF.term -> SLF.term -> SLF.term -> SLF.term =
  fun e e1 e' e2 p1 p2 h1 h2 h3 ->
    ! tc_joinprog_ctor 
      $$ tenv e $$ tenv e1 $$ tenv e' $$ tenv e2 
      $$ program p1 
      $$ program p2 $$ h1 $$ h2 $$ h3

(* 
            Γ ⊢ e : τ 
   ————————————————————————————– [tc_valdecl]
   Γ ⊢_d (x : τ) = e ⇒ Γ (x : τ) 
*)
let tc_valdecl 
: tenv -> identifier -> typ -> expression -> SLF.term -> SLF.term = 
  fun g x ty e h ->
    ! tc_valdecl_ctor $$ tenv g $$ identifier x $$ typ ty $$ expression e $$ h

(* 
   ——————————— [tc_eint]
   Γ ⊢ n : int
*)
let tc_eint : tenv -> int -> SLF.term = 
  fun e x -> 
    ! tc_eint_ctor $$ tenv e $ ilit x 
    
   
    
  
