
open ILF
open NLF

val term : NLF.signature -> SLF.term -> ILF.entity

val from_obj : ILF.obj -> SLF.term
val from_fam : ILF.fam -> SLF.term
val from_kind : ILF.kind -> SLF.term
