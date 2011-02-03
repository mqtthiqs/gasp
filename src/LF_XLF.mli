open NLF

val obj : XLF.args -> LF.obj -> XLF.obj
val fam : XLF.args -> LF.fam -> XLF.fam
val kind : LF.kind -> XLF.kind
val entry : (NLFSign.t -> XLF.entry -> NLFSign.entry) -> 
  NLFSign.t -> LF.entry -> NLFSign.entry

val from_obj : XLF.obj -> LF.obj
val from_fam : XLF.fam -> LF.fam
val from_kind : XLF.kind -> LF.kind
val from_sign : XLF.sign -> LF.sign
