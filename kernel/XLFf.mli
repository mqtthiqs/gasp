open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FAtom of subst * fconst * args

and obj =
  | Obj of subst * value
  | OBox of obj * position * obj

and args = value list

and value =
  | VHead of head
  | VLam of variable * obj

and def = head * args

and subst = (variable * def) list
