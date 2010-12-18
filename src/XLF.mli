type constant = string
type variable = string

type name =
  | Name of variable
  | Anonymous

type kind =
  | KType
  | KProd of name * fam * kind

and fam =
  | FProd of name * fam * fam
  | FConst of constant * args * kind

and obj =
  | OLam of name * fam * obj
  | OVar of variable * args * kind
  | OConst of constant * args * kind
  | OApp of obj * args * kind

and args = obj list
