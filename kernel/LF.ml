open Name

module type Sig = sig

  module Definitions : Definitions.Sig with type variable = Name.variable

  type head 

  type fhead 

  type fam =
    | FConst of fconst
    | FProd of variable * fam * fam
    | FApp of fhead * head list
    | FDef of (fam option, obj, fam) Definitions.construct

  and obj = 
    | OConst of oconst
    | OVar of variable
    | OLam of variable * fam * obj
    | OApp of head * head list
    | ODef of (fam option, obj, obj) Definitions.construct

  and kind =
    | KType
    | KProd of variable * fam * kind

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj

  type entry =
    | FDecl of fconst * kind
    | ODecl of oconst * fam
	
  type signature 

  val empty : unit -> signature

  val bind_oconst : oconst -> fam -> signature -> signature

  val bind_fconst : fconst -> kind -> signature -> signature

  val find_oconst : oconst -> signature -> fam

  val find_fconst : fconst -> signature -> kind

  val mem_oconst : oconst -> signature -> bool

  val mem_fconst : fconst -> signature -> bool

  val fold : (entry -> 'a -> 'a) -> signature -> 'a -> 'a

end

module Make (Head : sig 
  type head_
  type fhead_
end) : Sig with type head = Head.head_ and type fhead = Head.fhead_ 
= struct
  module Definitions = Definitions.Make (Name) 

  type head = Head.head_

  type fhead = Head.fhead_

  type fam =
    | FConst of fconst
    | FProd of variable * fam * fam
    | FApp of fhead * head list
    | FDef of (fam option, obj, fam) Definitions.construct

  and obj = 
    | OConst of oconst
    | OVar of variable
    | OLam of variable * fam * obj
    | OApp of head * head list
    | ODef of (fam option, obj, obj) Definitions.construct

  and kind =
    | KType
    | KProd of variable * fam * kind

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj

  type entry =
    | FDecl of fconst * kind
    | ODecl of oconst * fam

  type signature = kind Name.Fconstmap.t * fam Name.Oconstmap.t * entry list
      
  let empty () = Fconstmap.empty, Oconstmap.empty, []

  let bind_oconst x a (m, n, l) = m, Oconstmap.add x a n, ODecl (x, a) :: l

  let bind_fconst x a (m, n, l) = Fconstmap.add x a m, n, FDecl (x, a) :: l

  let find_oconst x (_, n, _) = Oconstmap.find x n

  let find_fconst x (m, _, _) = Fconstmap.find x m

  let mem_oconst x (_, n, _) = Oconstmap.mem x n

  let mem_fconst x (m, _, _) = Fconstmap.mem x m

  let fold f (_,_,l) acc = List.fold_left (fun acc e -> f e acc) acc l

end



