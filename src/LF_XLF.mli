
val obj : XLF.sign -> XLF.env -> LF.obj -> XLF.obj * XLF.fam
val fam : XLF.sign -> XLF.env -> LF.fam -> XLF.fam
val kind : XLF.sign -> XLF.env -> LF.kind -> XLF.kind
val sign : XLF.sign -> LF.sign -> XLF.sign

val from_obj : XLF.obj -> LF.obj
val from_fam : XLF.fam -> LF.fam
val from_kind : XLF.kind -> LF.kind
val from_sign : XLF.sign -> LF.sign
