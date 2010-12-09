type constant = string
type variable = string

type name' =
  | Name of variable
  | Anonymous

type name = name' Position.located

type fam' =
  | FConst of constant
  | FProd of name * fam * fam
  | FLam of name * fam * fam
  | FApp of fam * obj

and fam = fam' Position.located

and obj' = 
  | OConst of constant
  | OVar of variable
  | OLam of name * fam * obj
  | OApp of obj * obj

and obj = obj' Position.located

type kind' =
  | KType
  | KProd of name * fam * kind

and kind = kind' Position.located

type entry =
  | EKind of kind
  | EFam of fam

type signature = (constant * entry) list
type environ = (variable * fam) list

val sign_of_ast : LF_AST.signature -> signature
