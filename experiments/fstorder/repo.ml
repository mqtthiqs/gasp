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

module Env : sig
  type t
  val empty : t
  val find : int -> t -> LF.fam
  val add : LF.fam -> t -> t
end = struct
  type t = LF.fam list
  let empty = []
  let find x l = List.nth l x
  let add a l = (a :: l)
end

module Sign : sig
  type t
  val empty : t
  val ofind : OConst.t -> t -> LF.fam
  val ffind : FConst.t -> t -> LF.kind
end = struct
  module MO = Map.Make(OConst)
  module MF = Map.Make(FConst)
  type t = LF.fam MO.t * LF.kind MF.t
  let empty = MO.empty, MF.empty
  let ofind x ((o, f):t) = MO.find x o
  let ffind x ((o, f):t) = MF.find x f
end

type t = {
  sign: Sign.t;
  ctx: Context.t;
  head: Meta.t
}

let init sign : t = {
  sign = sign;
  ctx = Context.empty;
  head = Meta.make "empty"
}

let load f : t =
  let ch = open_in f in
  Marshal.from_channel ch

let save f (r : t) =
  let ch = open_out f in
  Marshal.to_channel ch r
