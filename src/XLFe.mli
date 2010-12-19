open Name

type khead =
  | KType

type kind =
  | KHead of khead
  | KProd of name * fam * kind

and fhead =
  | FConst of constant * args * khead

and fam =
  | FProd of name * fam * fam
  | FHead of fhead

and ohead =
  | OVar of variable * args * fhead
  | OConst of constant * args * fhead
  | OApp of obj * args * fhead

and obj =
  | OLam of name * fam * obj
  | OHead of ohead

and args = obj list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list
