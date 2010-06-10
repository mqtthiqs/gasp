type declaration =
  | DValue of identifier * ty * expression
  | DType  of type_identifier

and declarations = declaration list

and expression = 
  | Var of identifier
  | Lam of identifier * ty * expression
  | App of expression * expression

and ty = 
  | TyVar   of type_identifier
  | TyArrow of ty * ty

and identifier = string

and type_identifier = string

type typing_environment = 
    Env of bindings

and fragment = Fragment of typing_environment * declarations

and binding = 
  | BindVar   of identifier * ty
  | BindTyVar of type_identifier

and bindings = binding list

type expression_typing_judgment = 
    TypeExp of typing_environment * expression * ty

type module_typing_judgment = {
  in_env  : typing_environment;
  decs    : declarations;
  out_env : typing_environment
}



