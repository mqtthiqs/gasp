open LF
open Struct

val push : Repo.t -> Env.t -> head * spine -> Repo.t * int
val pull : Repo.t -> Names.Meta.t -> obj
val init : Repo.t -> SLF.sign -> Repo.t

module Conv : sig
  exception Not_conv_obj of Repo.t * obj * obj
  exception Not_conv_fam of Repo.t * fam * fam
  (* debugging only *)
  val obj : Repo.t -> obj * obj -> unit
end

(* debugging only *)
module Check : sig
  exception Non_functional_fapp of Repo.t * Env.t * spine
  exception Non_functional_app of Repo.t * Env.t * spine * fam

  val obj : Repo.t -> Env.t -> obj * fam -> Repo.t * obj
end
