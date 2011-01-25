open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of constant * args * kind

and obj =
  | OLam of variable * fam * obj
  | OVar of variable * args * fam
  | OConst of constant * args * fam
  | OApp of obj * args * fam
  | OMeta of NLF.variable * args * fam

and args = (variable * obj) list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list
