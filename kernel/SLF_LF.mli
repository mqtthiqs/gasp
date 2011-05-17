open NLF

val term : NLFSign.t -> SLF.term -> LF.entity
(* val sign : (NLFSign.t -> LF.entry -> NLF.entry) ->  *)
(*   NLFSign.t -> SLF.sign -> NLFSign.t *)

val from_obj : LF.obj -> SLF.term
val from_fam : LF.fam -> SLF.term
val from_kind : LF.kind -> SLF.term
