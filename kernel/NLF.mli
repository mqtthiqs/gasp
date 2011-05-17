open Name

module rec NLF : sig

  type kind =
    | KType
    | KProd of variable * fam * kind

  and fam =
    | FProd of variable * fam * fam
    | FHead of subst * fconst * args

  and obj =
    | Obj of subst * value

  and args = value list

  and vhead = XLF.ohead

  and value =
    | VHead of vhead * fconst * args
    | VLam of variable * fam * obj

  and def =
    | DApp of vhead * args * fconst * args (* vhead bindé ds repo *)
    | DHead of vhead * fam		    (* vhead bindé ds env *)

  and subst = def NLFSubst.t

  type entry =
    | FDecl of fconst * kind
    | ODecl of oconst * fam
end

and NLFSubst : (Map.S with type key = variable)
and NLFSign : sig
  module FDecl : Map.S with type key = fconst
  module ODecl : Map.S with type key = oconst
  type t = NLF.kind FDecl.t * NLF.fam ODecl.t
  val fold : (NLF.entry -> 'a -> 'a) -> t -> 'a -> 'a
  val empty : t
end

open Print

module Pp : sig
  open NLF
  open Format
  val sign : NLFSign.t printing_fun
  val obj : obj printing_fun
  val fam : fam printing_fun
  val kind : kind printing_fun
end

val go : NLF.obj -> position -> NLF.def -> NLF.obj
val lift_def : variable -> NLF.obj -> NLF.fam
val to_def : NLF.obj -> NLF.def
val bidon : NLF.obj
