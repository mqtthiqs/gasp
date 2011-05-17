open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of constant * args

and obj =
  | OLam of variable * obj
  | OHead of ohead * args
  | OBox of obj * position * obj

and ohead = 
  | HVar of variable
  | HConst of constant

and args = obj list

type entry =
  | FDecl of kind
  | ODecl of fam

type env = (variable * fam) list
