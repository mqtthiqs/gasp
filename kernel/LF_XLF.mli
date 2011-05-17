open NLF

val obj : LF.obj -> XLF.obj
val fam : LF.fam -> XLF.fam
val kind : LF.kind -> XLF.kind
val entry : (NLFSign.t -> XLF.entry -> NLF.entry) -> 
  NLFSign.t -> LF.entry -> NLF.entry

val from_obj : XLF.obj -> LF.obj
val from_fam : XLF.fam -> LF.fam
val from_kind : XLF.kind -> LF.kind
