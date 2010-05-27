open AST
open Format

let ( ! ) = Position.value

let rec term fmt = function
  | Var x -> fprintf fmt "@[%s@]" x
  | App (t, x) -> fprintf fmt "@[%a @,@[%s@]@]" term t x

let rec sort fmt = function
  | KType -> fprintf fmt "Type"
  | KKind -> fprintf fmt "Kind"

let rec head fmt = function
  | Term t -> 
      fprintf fmt "@[%a@]" term t
  | Sort s ->
      fprintf fmt "@[%a@]" sort s

let rec ptype fmt = function
  | Head h -> 
      fprintf fmt "@[%a@]" head h
  | Prod (x, t, s) -> 
      fprintf fmt "@[@[(%s : %a).@]@,@[%a@]@]" x ptype' t ptype' s
  | SProd (x, a, s) -> 
      fprintf fmt "@[@[(%s = %a).@]@,%a@]" x term !a ptype' s

and ptype' fmt t = ptype fmt !t

let term fmt t = fprintf fmt "@[%a@]" term t

let ptype fmt t = fprintf fmt "@[%a@]" ptype t

let rec env fmt e =
  try
    let (e,x) = Env.pop_decl e in
    judg fmt (Env.lookup e x); 
    ignore (Env.pop_decl e);
    fprintf fmt ", "; env fmt e
  with Env.Empty -> ()

and judg fmt (e,h:Env.j) : unit = 
  try ignore (Env.pop_decl e);
    fprintf fmt "@[(%a ⊢ %a)@]" env e head h
  with Env.Empty -> 
    fprintf fmt "@[%a@]" head h
