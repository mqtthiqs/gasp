open Name

type fam = XLF.fam

type fhead = fconst * XLF.args

and obj =
  | Obj of subst * value

and args = (variable * value) list

and ohead = XLF.ohead

and value =
  | VHead of ohead * fhead
  | VLam of variable * fam * obj

and def =
  | DAtom of ohead * args * fhead (* ohead bindé ds repo *)
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
