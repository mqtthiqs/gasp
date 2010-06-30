type id = Name.t

type term = 
  | Var of id
  | App of term' * id
  | Unknown 

and term' = term Position.located

type sort = 
  | KType
  | KKind

type ptype = 
  | Term  of term'
  | Sort  of sort
  | Prod  of id * ptype' * ptype'
  | SProd of id * ptype' * term' * ptype'
  | Cont

and ptype' = ptype Position.located

type patch = Patch of ptype'

