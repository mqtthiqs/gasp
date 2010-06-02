open AST
open Format

let ( ! ) = Position.value

let id fmt x = 
  fprintf fmt "%s" (Name.to_string x)

let rec term fmt = function
  | Var x -> fprintf fmt "@[%a@]" id x
  | App (t, x) -> fprintf fmt "@[%a @,@[%a@]@]" term' t id x

and term' fmt t = term fmt !t

let rec sort fmt = function
  | KType -> fprintf fmt "Type"
  | KKind -> fprintf fmt "Kind"

let rec ptype fmt = function
  | Term t -> 
      fprintf fmt "@[%a@]" term' t
  | Sort s ->
      fprintf fmt "@[%a@]" sort s
  | Prod (x, t, s) -> 
      fprintf fmt "@[@[(%a : %a).@]@,@[%a@]@]" id x ptype' t ptype' s
  | SProd (x, t, a, s) -> 
      fprintf fmt "@[@[(%a = %a : %a).@]@,%a@]" id x term' a ptype' t ptype' s

and ptype' fmt t = ptype fmt !t

let term fmt t = fprintf fmt "@[%a@]" term t

let ptype fmt t = fprintf fmt "@[%a@]" ptype t

(* let subst fmt s = *)
(*   Subst.fold (fun n k () -> fprintf fmt "@[(%s -> %d)@]" n (Obj.magic k)) s (); *)
