open Name

type kind =
  | KType
  | KProd of variable * fam * kind

and fam =
  | FProd of variable * fam * fam
  | FConst of fconst * args

and obj =
  | OLam of variable * obj
  | OHead of ohead * args
  | OBox of obj * position * obj

and ohead = 
  | HVar of variable
  | HConst of oconst

and args = obj list

type entry =
  | FDecl of fconst * kind
  | ODecl of oconst * fam

module Sign : sig
  type t = kind Name.Fconstmap.t * fam Name.Oconstmap.t
  val fold : (entry -> 'a -> 'a) -> t -> 'a -> 'a
  val empty : t
end
