open AST
open Env

let ( ! ) = Position.value
let pos_of = Position.position

let prod_rule = function
  | _,s -> s				(* PTS total *)

let axiom_rule = function
  | KType -> KKind
  | _ -> raise Not_found

(* 
 * Typing errors
 *)

let type_error pos msg = 
  let as_string f = 
    let b = Buffer.create 13 in 
    let fmt = Format.formatter_of_buffer b in
    f fmt;
    Format.pp_print_flush fmt ();
    Buffer.contents b in
  Error.error "during type checking" pos (as_string msg)

let error_not_equal pos jx jy =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[%a <> %a.@]@." Print.judg jx Print.judg jy)

let error_not_a_product pos a x =
  type_error pos
    (fun fmt -> Format.fprintf fmt 
       "@[%s cannot be applied to %a.@]@." x Print.term a)

let error_not_a_sort pos a =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[The type of %a is not a sort.@]@." Print.term a)

let error_not_bound pos x =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Variable %s is not bound.@]@." x)

let error_prod_rule pos s1 s2 =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[Product (%a,%a,_) is not available.@]@." Print.sort s1 Print.sort s2)

let error_type_kind pos s =
  type_error pos
    (fun fmt -> Format.fprintf fmt "@[%a has no type.@]@." Print.sort s)

(* 
 * Type-checking
 *)

(* let has_args e = *)
(*   try ignore (Env.pop_decl e); true *)
(*   with Env.Empty -> false *)

(* let rec equals_term e1 e2 t1 t2 = *)
(*   match t1,t2 with *)
(*     | Var x, Var y -> Env.equal e1 e2 x y *)
(*     | App (a,x), App (b,y) -> Env.equal e1 e2 x y && equals_term e1 e2 a b *)
(*     | _ -> false *)

(* let rec equals_env e1 e2 = *)
(*   try *)
(*     let (e1,x1) = Env.pop_decl e1 in *)
(*     let (e2,x2) = Env.pop_decl e2 in *)
(*     equals_env e1 e2 && *)
(*       equals_judg (Env.lookup e1 x1) (Env.lookup e2 x2) *)
(*   with Env.Empty ->  *)
(*     if has_args e1 || has_args e2 then false else true (\* TODO ya mieux *\) *)

(* and equals_judg (e1,h1) (e2,h2) = *)
(*   equals_env e1 e2 && *)
(*     match h1,h2 with *)
(*       | Term a, Term b -> equals_term e1 e2 a b *)
(*       | Sort s, Sort t -> s=t *)
(*       | _ -> false *)

(* key -> key substitutions *)
module Subst = struct
  open Env
  type t = key Keymap.t
  let empty = Keymap.empty
  let bind sigma k l = Keymap.add k l sigma
  let lookup sigma k = Keymap.find k sigma
end

(* id -> key substitutions *)
module Topsubst = struct
  open Env
  type t = key Idmap.t
  let empty = Idmap.empty
  let bind sigma k l = Idmap.add k l sigma
  let lookup sigma k = Idmap.find k sigma
end

let sort_of j t = 
  match j.head with 
    | Hsort s -> s 
    | _ -> error_not_a_sort (pos_of t) !t

let rec infer_term top sigma env = function
  | Var x -> 
      begin try Env.lookup env (Topsubst.lookup top !x) 
      with Not_found -> error_not_bound (pos_of x) !x end
  | App (a,x) -> 
      let jx = Env.lookup env (Topsubst.lookup top !x) in
      let ja = infer_term top sigma env !a in
      let ea,ky = 
	try Env.pop_decl ja.env
	with Env.Empty -> error_not_a_product (pos_of a) !a !x in
      let jy = Env.lookup env ky in
      if jx != jy then error_not_equal (pos_of x) jx jy; (* TODO *)
      { env = ea; 
	head = ja.head;
	sort = ja.sort }		(* TODO subst *)
  | Sort s ->
      begin try { env = env; head = Hsort !s; sort = axiom_rule !s }
      with Not_found -> error_type_kind (pos_of s) !s end
  | Prod (x,t,u) -> 
      let jt = infer_term top sigma (Env.clear_decl env) !t in
      let s1 = sort_of jt t in
      Format.printf "intro dec %a : %a@\n" Print.name x Print.judg jt;
      let (env,kx) = Env.bind_decl env jt in
      let top = match x with Id x -> Topsubst.bind top x kx | Anonymous -> top in
      let ju = infer_term top sigma env !u in
      let s2 = sort_of ju u in
      begin try { ju with sort = prod_rule (s1,s2) }
      with Not_found -> error_prod_rule (pos_of t) s1 s2 end
  | SProd (x,a,u) ->
      Format.printf "typage de (%s = %a)@\n" x Print.term !a;
      let ja = 
	try Env.lookup env (Topsubst.lookup top x)
	with Not_found -> infer_term top sigma (Env.clear_decl env) !a in
      let s1 = sort_of ja a in
      Format.printf "intro def %s : %a@\n" x Print.judg ja;
      let env,kx = Env.bind_def env (Env.keys_of top !a) ja in
      let top = Topsubst.bind top x kx in
      let ju = infer_term top sigma env !u in
      let s2 = sort_of ju u in
      begin try { ju with sort =  prod_rule (s1,s2) }
      with Not_found -> error_prod_rule (pos_of a) s1 s2 end

let infer_term env t = infer_term Topsubst.empty Subst.empty env !t
