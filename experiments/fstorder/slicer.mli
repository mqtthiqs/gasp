open Struct

val init : SLF.sign -> repo
val commit : repo -> env -> SLF.term -> repo
val checkout : repo -> SLF.term
