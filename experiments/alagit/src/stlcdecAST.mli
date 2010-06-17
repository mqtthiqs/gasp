type 'a meta = 'a MetaAST.t

type declaration =
  | DValue of identifier' * ty' * expression'
  | DType  of type_identifier'

and declarations = 
  | EmptyDeclarations 
  | ConsDeclaration of declaration' * declarations'

and expression = 
  | Var of identifier'
  | Lam of identifier' * ty' * expression'
  | App of expression' * expression'

and ty = 
  | TyVar   of type_identifier'
  | TyArrow of ty' * ty'

and identifier = string

and type_identifier = string

and typing_environment = 
    Env of bindings'

and fragment = Fragment of typing_environment' * declarations'

and binding = 
  | BindVar   of identifier' * ty'
  | BindTyVar of type_identifier'

and bindings = 
  | NoBinding 
  | ConsBinding of binding' * bindings'

and declaration' = declaration meta
and declarations' = declarations meta
and expression' = expression meta
and ty' = ty meta
and identifier' = identifier meta
and type_identifier' = type_identifier meta
and typing_environment' = typing_environment meta
and bindings' = bindings meta
and binding' = binding meta

type expression_typing_judgment = 
    TypeExp of typing_environment * expression * ty

type module_typing_judgment = {
  in_env  : typing_environment;
  decs    : declarations;
  out_env : typing_environment
}



