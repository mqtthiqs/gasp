open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fhead =
  | FConst of constant * args

and fam =
  | FProd of variable * fam * fam
  | FHead of fhead

and ohead =
  | HVar of variable
  | HMeta of NLF.variable
  | HConst of constant

and obj =
  | OLam of variable * fam * obj
  | OHead of ohead * args * fhead

and args = (variable * obj) list

type entry =
  | FDecl of kind
  | ODecl of fam

type sign = (constant * entry) list
type env = (variable * fam) list
