
module type NameSig = sig
  include Map.OrderedType
  val make : string -> t
  val repr : t -> string
end

module Meta : NameSig = struct
  type t = string
  let compare = String.compare
  let make x = x
  let repr x = x
end

module OConst : NameSig = struct
  type t = string
  let compare = String.compare
  let make x = x
  let repr x = x
end

module OConstSet : Set.S with type elt = OConst.t = Set.Make(OConst)

module FConst : NameSig = struct
  type t = string
  let compare = String.compare
  let make x = x
  let repr x = x
end
