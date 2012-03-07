open LF
open Struct

val push : repo -> env -> head * spine -> repo * subst
val pull : repo -> Names.Meta.t -> obj
val init : repo -> SLF.sign -> repo

module Conv : sig
  exception Not_conv_obj of repo * env * obj * obj
  exception Not_conv_fam of repo * env * fam * fam
  (* debugging only *)
  val obj : repo -> env -> obj * obj * fam -> unit
end

(* debugging only *)
module Check : sig
  exception Non_functional_fapp of repo * env * spine
  exception Non_functional_app of repo * env * spine * fam

  val obj : repo -> env -> obj * fam -> repo * obj
end
