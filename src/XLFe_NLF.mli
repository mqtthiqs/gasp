open Name
open NLF

val obj : NLF.env -> XLFe.obj -> NLF.obj
val fam : NLF.env -> XLFe.fam -> NLF.fam
val kind : NLF.env -> XLFe.kind -> NLF.kind
val sign : NLF.sign -> XLFe.sign -> NLF.sign

val from_sign : NLF.sign -> XLFe.sign
