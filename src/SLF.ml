
type ident = string

type term' = 
  | Type
  | Prod of ident * term * term
  | Arr of term * term
  | Lam of ident * term * term
  | App of term * term
  | Var of ident

and term = term' Position.located

type entry =
  | Decl of term

type sign = (ident * entry) list

let rec equals_term t u = 
  match Position.value t, Position.value u with
    | Type, Type -> true
    | Prod(x,a,b), Prod(x',a',b') ->
	x=x' && equals_term a a' && equals_term b b'
    | Arr(t,u), Arr(t',u') -> equals_term t t' && equals_term u u'
    | App(t,u), App(t',u') -> equals_term t t' && equals_term u u'
    | Var x, Var x' -> x = x'
    | _ -> false

let equals_sign s s' = 
  try List.for_all2 (fun (x, Decl t) (x',Decl t') -> x=x' && equals_term t t') s s'
  with Invalid_argument "List.for_all2" -> false
