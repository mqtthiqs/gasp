open Names
open LF

module Env = struct
  type t = (string option * fam) list
  let empty = []
  let length = List.length
  let find x l = snd (List.nth l x)
  let add x a l = ((x, a) :: l)
  let to_list l = l
  let names_of env = fst (List.split (to_list env))

end

module Context = struct

  module M = Map.Make(Meta)
  type t = (Env.t * obj * fam) M.t
  let empty = M.empty
  let find = M.find
  let add = M.add
  let fold = M.fold

end

module rec Sign : sig
  open LF

  type  entry_type =
    | Sliceable
    | Non_sliceable
    | Defined of (Repo.t -> Env.t -> obj list -> obj)

  type t
  val empty : t
  val ofind : OConst.t -> t -> fam * entry_type
  val ffind : FConst.t -> t -> kind
  val oadd : OConst.t -> fam * entry_type -> t -> t
  val fadd : FConst.t -> kind -> t -> t
  val fold :
    (OConst.t -> fam * entry_type -> 'a -> 'a) ->
    (FConst.t -> kind -> 'a -> 'a) -> t -> 'a -> 'a
end = struct
  open LF

  type entry_type =
    | Sliceable
    | Non_sliceable
    | Defined of (Repo.t -> Env.t -> obj list -> obj)

  module MO = Map.Make(Names.OConst)
  module MF = Map.Make(Names.FConst)

  type t = (fam * entry_type) MO.t * kind MF.t
  let empty = MO.empty, MF.empty
  let ofind x ((o, f):t) = MO.find x o
  let ffind x ((o, f):t) = MF.find x f
  let oadd x a ((o, f):t) = MO.add x a o, f
  let fadd x k ((o, f):t) = o, MF.add x k f
  let fold f1 f2 ((o, f):t) (acc : 'a) : 'a = MO.fold f1 o (MF.fold f2 f acc)
end

and Repo : sig
  type t = {
    sign: Sign.t;
    ctx: Context.t;
    head: Names.Meta.t;
  }

  val empty : t
end = struct

  type t = {
    sign: Sign.t;
    ctx: Context.t;
    head: Meta.t;
  }

  let empty = {sign = Sign.empty; ctx = Context.empty; head = Meta.make "DUMMY"}

  module Printer = struct
  end

end
    
type env = Env.t
type sign = Sign.t
type repo = Repo.t
