open NLF

val term : NLF.sign -> SLF.term -> LF.entity
val sign : NLF.sign -> 
  (NLF.sign -> LF.entry -> NLFSign.entry) -> 
  SLF.sign -> NLFSign.t

val from_obj : LF.obj -> SLF.term
val from_fam : LF.fam -> SLF.term
val from_kind : LF.kind -> SLF.term
val from_sign : LF.sign -> SLF.sign
