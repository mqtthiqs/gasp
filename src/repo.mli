type t

val init : SLF.sign -> t
val compile : t -> SLF.term -> t
val show : t -> unit

val load : unit -> t
val save : t -> unit
