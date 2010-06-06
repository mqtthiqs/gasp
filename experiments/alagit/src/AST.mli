type id = string

type name =
  | Id of id
  | Anonymous

type id' = id Position.located

type sort = 
  | KType
  | KKind

type sort' = sort Position.located

type term = 
  | Var of id'
  | App of term' * id'
  | Sort of sort'
  | Prod  of name * term' * term'
  | SProd of id * term' * term'

and term' = term Position.located

type patch = Patch of term'
