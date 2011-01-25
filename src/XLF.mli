open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of constant * args

and obj =
  | OLam of variable * fam * obj
  | OVar of NLF.variable * args
  | OMeta of variable * args
  | OConst of constant * args
  | OApp of obj * args

and args = obj list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list
