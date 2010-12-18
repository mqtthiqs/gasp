open Name

type kind =
  | KType
  | KProd of name * fam * kind

and fam =
  | FProd of name * fam * fam
  | FConst of constant * args * kind

and obj =
  | OLam of name * fam * obj
  | OVar of variable * args * fam
  | OConst of constant * args * fam
  | OApp of obj * args * fam

and args = obj list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list
