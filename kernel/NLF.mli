open Name

type fam =
  | FAtom of fatom
  | FProd of variable * fam * fam

and fatom = subst * fconst * args

and obj =
  | Obj of subst * value

and args = value list

and value =
  | VHead of head * fatom
  | VLam of variable * fam * obj

and def =
  | DAtom of head * args * fatom           (* head bindé ds repo *)
  | DHead of head * fam		    (* head bindé ds env *)

and subst = def Varmap.t

type kind =
  | KProd of variable * fam * kind
  | KType

type entry =
  | FDecl of fconst * kind
  | ODecl of oconst * fam
