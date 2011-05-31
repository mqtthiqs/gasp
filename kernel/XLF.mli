open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fatom = fconst * args

and fam =
  | FProd of variable * fam * fam
  | FAtom of fatom

and obj =
  | OLam of variable * obj
  | OAtom of ohead * args
  | OBox of obj * position * obj

and ohead = 
  | HVar of variable
  | HConst of oconst

and args = obj list
