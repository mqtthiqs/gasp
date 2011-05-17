open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of fconst * args

and obj =
  | OLam of variable * obj
  | OHead of ohead * args
  | OBox of obj * position * obj

and ohead = 
  | HVar of variable
  | HConst of oconst

and args = obj list
