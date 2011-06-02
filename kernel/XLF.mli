open Name

type kind =
  | KType
  | KProd of name * fam * kind

and fatom = fconst * args

and fam =
  | FProd of name * fam * fam
  | FAtom of fatom

and obj =
  | OLam of name * obj
  | OAtom of head * args
  | OBox of obj * position * obj

and args = obj list
