type id = string

type sort = 
  | KType
  | KKind

type term = 
  | Var of id
  | App of term' * id
  | Sort of sort
  | Prod  of id * term' * term'
  | SProd of id * term' * term'

and term' = term Position.located

type patch = Patch of term'
