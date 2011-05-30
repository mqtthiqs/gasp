open Name

type fam = value Varmap.t * XLF.fam
and fatom = value Varmap.t * XLF.fatom

and obj =
  | Obj of subst * value

and args = (variable * value) list

and ohead = XLF.ohead

and value =
  | VHead of ohead * fatom
  | VLam of variable * fam * obj

and def =
  | DAtom of ohead * args * fatom           (* ohead bindé ds repo *)
  | DHead of ohead * fam		    (* ohead bindé ds env *)

and subst = def Varmap.t

module Pp : sig
  open Print
  val obj : obj printing_fun
end

val go : obj -> position -> value
val bind : variable -> def -> obj -> obj
val lift_def : variable -> obj -> fam
val to_def : obj -> def
