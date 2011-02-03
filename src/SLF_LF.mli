open NLF

val term : NLF.sign -> SLF.term -> LF.entity
val sign : (NLFSign.t -> LF.entry -> NLFSign.entry) -> 
  NLFSign.t -> SLF.sign -> NLFSign.t

val from_obj : LF.obj -> SLF.term
val from_fam : LF.fam -> SLF.term
val from_kind : LF.kind -> SLF.term
val from_sign : LF.sign -> SLF.sign
