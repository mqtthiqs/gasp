open Util
open Names
open LF

module Env = struct
  type t = (binder * fam) list
  let find x l = snd (List.nth l x)
  let add x a l = ((x, a) :: l)
  let names_of env = fst (List.split env)
end

module Context = struct

  module M = Map.Make(Meta)
  type t = (Env.t * obj * fam) M.t * int
  let empty = M.empty, 0
  let find x ((t, _) : t) = M.find x t
  let add x (e,m,a) (t, n) = M.add x (e,m,a) t, n
  let fold f ((t, n):t) acc = M.fold f t acc
  let fresh (r, n) =
    Meta.make ("X"^string_of_int n),
    (r, succ n)

end

module rec Sign : sig
  open LF

  type  entry_type =
    | Sliceable
    | Non_sliceable
    | Defined of (Repo.t -> Env.t -> (Repo.t -> Env.t -> obj -> Repo.t * obj) -> spine -> Repo.t * obj)

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
    | Defined of (Repo.t -> Env.t -> (Repo.t -> Env.t -> obj -> Repo.t * obj) -> spine -> Repo.t * obj)

  module MO = Map.Make(OConst)
  module MF = Map.Make(FConst)

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
    head: Meta.t * subst;
  }

  val empty : t
  val fresh : t -> Meta.t * t
end = struct

  type t = {
    sign: Sign.t;
    ctx: Context.t;
    head: Meta.t * subst;
  }

  let empty = {sign = Sign.empty; ctx = Context.empty; head = Meta.make "DUMMY", []}
  let fresh repo =
    let x, ctx = Context.fresh repo.ctx in
    x, {repo with ctx = ctx}

  module Printer = struct
  end

end
    
type env = Env.t
type sign = Sign.t
type repo = Repo.t

module Renaming = struct

  type t = int list

  let dummy_var = 42

  let inverse = List.transpose @> List.map @@ Option.default dummy_var

  let subst_of env s = List.map
    (fun x ->
      if x <> dummy_var then LFUtil.eta_expand_var x (snd (List.nth env x))
      else inj @@ OApp (HVar x, [])
    ) s

  let drop_env s (e : env) : env =
    List.drop (fun n (x, m) -> x, Lower.fam n m) e s

end
