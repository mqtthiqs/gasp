open Format

open AST

let ( ! ) = Position.value

let rec sort fmt = function
  | KType -> fprintf fmt "Type"
  | KKind -> fprintf fmt "Kind"

let rec term fmt : term -> unit = function
  | Var x -> fprintf fmt "@[%s@]" !x
  | App (t, x) -> fprintf fmt "@[%a @,@[%s@]@]" term' t !x
  | Sort s ->
      fprintf fmt "@[%a@]" sort !s
  | Prod (x, t, s) -> 
      fprintf fmt "@[@[(%s : %a).@]@,@[%a@]@]" x term' t term' s
  | SProd (x, a, s) -> 
      fprintf fmt "@[@[(%s = %a).@]@,%a@]" x term' a term' s

and term' fmt (t:term') = term fmt !t

open Env

let rec head fmt = function
  | Happ (h, x) -> fprintf fmt "@[%a @,@[%d@]@]" head h (Obj.magic x) (* TODO *)
  | Hsort s -> sort fmt s

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
