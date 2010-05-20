open AST
open Format

let ( ! ) = Position.value

let rec term fmt = function
  | Var x -> fprintf fmt "@[%s@]" x
  | App (t, x) -> fprintf fmt "@[%a @,@[%s@]@]" term' t x

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
      fprintf fmt "@[@[(%s : %a).@]@,@[%a@]@]" x ptype' t ptype' s
  | SProd (x, t, a, s) -> 
      fprintf fmt "@[@[(%s = %a : %a).@]@,%a@]" x term' a ptype' t ptype' s

and ptype' fmt t = ptype fmt !t

let term fmt t = fprintf fmt "@[%a@]" term t

let ptype fmt t = fprintf fmt "@[%a@]" ptype t

let subst fmt s =
  Subst.fold (fun n k () -> fprintf fmt "@[(%s -> %d)@]" n k) s ();
