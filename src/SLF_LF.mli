
val term : LF.sign -> LF.env -> SLF.term -> LF.entity
val sign : LF.sign -> SLF.sign -> LF.sign

val from_obj : LF.obj -> SLF.term
val from_fam : LF.fam -> SLF.term
val from_kind : LF.kind -> SLF.term
val from_sign : LF.sign -> SLF.sign
