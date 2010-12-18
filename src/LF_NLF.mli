open NLF

val kind_to_kind : LF.sign -> LF.kind -> NLF.kind
val fam_to_fam : LF.sign -> NLF.env -> LF.fam -> NLF.fam
val obj_to_obj : LF.sign -> NLF.env -> LF.obj -> NLF.obj
val sign_to_sign : LF.sign -> NLFSign.t
