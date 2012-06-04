
module type NameSig = sig
  include Map.OrderedType
  val make : string -> t
  val repr : t -> string
  val print : Format.formatter -> t -> unit
  val hash : t -> int
end

module Meta : NameSig = struct
  type t = string
  let compare = String.compare
  let make x = x
  let repr x = x
  let print fmt x = Format.fprintf fmt "?%s" x
  let hash = Hashtbl.hash
end

module OConst : NameSig = struct
  type t = string
  let compare = String.compare
  let make x = x
  let repr x = x
  let print fmt x = Format.fprintf fmt "%s" x
  let hash = Hashtbl.hash
end

module OConstSet : Set.S with type elt = OConst.t = Set.Make(OConst)

module FConst : NameSig = struct
  type t = string
  let compare = String.compare
  let make x = x
  let repr x = x
  let print fmt x = Format.fprintf fmt "%s" x
  let hash = Hashtbl.hash
end
