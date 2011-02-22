open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of constant * args * kind

and obj =
  | OLam of variable * fam * obj
  | OHead of ohead * args * fam

and ohead =
  | HVar of variable
  | HConst of constant
  | HApp of obj
  | HMeta of NLF.variable

and args = (variable * obj) list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list
