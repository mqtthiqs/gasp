type id = string

type term = 
  | Var of id
  | App of term * id

type sort = 
  | KType
  | KKind

type head =
  | Sort of sort
  | Term of term

type ptype = 
  | Head  of head
  | Prod  of id * ptype' * ptype'
  | SProd of id * term Position.located * ptype'

and ptype' = ptype Position.located

type patch = Patch of ptype'
