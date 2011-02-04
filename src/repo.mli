type t

val init : SLF.sign -> t
val commit : t -> SLF.term -> t
val checkout : t -> unit
val show : t -> unit
val check : t -> unit

val load : unit -> t
val save : t -> unit
