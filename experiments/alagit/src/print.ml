open Format

open AST

let ( ! ) = Position.value

let rec sort fmt = function
  | KType -> fprintf fmt "Type"
  | KKind -> fprintf fmt "Kind"

let name fmt = function
  | Id x -> fprintf fmt "%s" x
  | Anonymous -> fprintf fmt "_"

let rec term fmt : term -> unit = function
  | Var x -> fprintf fmt "@[%s@]" !x
  | App (t, x) -> fprintf fmt "@[%a @,@[%s@]@]" term' t !x
  | Sort s ->
      fprintf fmt "@[%a@]" sort !s
  | Prod (Id x, t, s) -> 
      fprintf fmt "@[@[(%s : %a).@]@,@[%a@]@]" x term' t term' s
  | Prod (Anonymous, t, s) -> 
      fprintf fmt "@[(@[%a@] -> @[%a@])@]" term' t term' s
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

and judg fmt (j:Env.j) : unit =
  try ignore (Env.pop_decl j.env);
    fprintf fmt "@[(%a ⊢ %a : %a)@]" env j.env head j.head sort j.sort
  with Env.Empty ->
    fprintf fmt "@[%a : %a@]" head j.head sort j.sort
