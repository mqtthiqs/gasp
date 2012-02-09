open Names

module Context : sig
  type t
  val empty : t
  val find : Meta.t -> t -> LF.obj * LF.fam
  val add : Meta.t -> LF.obj * LF.fam -> t -> t
end = struct
  module M = Map.Make(Meta)
  type t = (LF.obj * LF.fam) M.t
  let empty = M.empty
  let find = M.find
  let add = M.add
end

type t = {
  sign: LF.Sign.t;
  ctx: Context.t;
  head: Meta.t;
  bound: OConstSet.t
}

let load f : t =
  let ch = open_in f in
  Marshal.from_channel ch

let save f (r : t) =
  let ch = open_out f in
  Marshal.to_channel ch r
