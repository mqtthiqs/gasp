type id = string

type term = 
  | Var of id
  | App of term' * id

and term' = term Position.located

type sort = 
  | KType

type ptype = 
  | Term  of term'
  | Sort  of sort
  | Prod  of id * ptype' * ptype'
  | SProd of id * ptype' * term' * ptype'

and ptype' = ptype Position.located

type patch = Patch of ptype'

type entry = 
    | Dec of ptype
    | Def of ptype * term
	
