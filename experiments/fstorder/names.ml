
module Meta : sig
  include Map.OrderedType
  val make : string -> t
  val repr : t -> string
end = struct
  type t = string
  let compare = String.compare
  let make x = x
  let repr x = x
end

module OConst : Map.OrderedType = struct
  type t = string
  let compare = String.compare
end

module FConst : Map.OrderedType = struct
  type t = string
  let compare = String.compare
end
