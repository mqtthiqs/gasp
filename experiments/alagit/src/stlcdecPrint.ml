open StlcdecAST
open Format

let rec declaration fmt = function
  | DValue (x, t, e) -> 
    fprintf fmt "@[let %a : %a = @,@[%a@]@]" 
      identifier' x
      ty' t
      expression' e
  | DType tvar -> 
    fprintf fmt "@[let type %a@]" 
      type_identifier' tvar

and declarations fmt = function
  | EmptyDeclarations -> ()
  | ConsDeclaration (d, ds) -> fprintf fmt "@[%a@\n%a@]" declaration' d declarations' ds

and expression fmt = function
  | Var x -> 
    fprintf fmt "%a" identifier' x
  | Lam (x, t, e) -> 
    fprintf fmt "(fun (%a: %a) -> %a)" 
      identifier' x
      ty' t
      expression' e
  | App (e1, e2) -> 
    fprintf fmt "(%a %a)" 
      expression' e1
      expression' e2

and ty fmt = function
  | TyVar x -> 
    fprintf fmt "%a" type_identifier' x
  | TyArrow (ty1, ty2) -> 
    fprintf fmt "(%a -> %a)" 
      ty' ty1
      ty' ty2

and identifier fmt x = 
  fprintf fmt "%s" x

and type_identifier fmt x = 
  fprintf fmt "%s" x

and typing_environment fmt (Env bs) = 
  bindings' fmt bs

and fragment fmt (Fragment (tenv, decs)) = 
  fprintf fmt "@[%a@\n@\n%a@]@."
    typing_environment' tenv
    declarations' decs

and binding fmt = function
  | BindVar (x, t) -> 
    fprintf fmt "@[val %a : %a@]" 
      identifier' x
      ty' t

  | BindTyVar tvar -> 
    fprintf fmt "@[type %a@]" 
      type_identifier' tvar

and bindings fmt = function
  | NoBinding -> ()
  | ConsBinding (b, bs) -> fprintf fmt "@[%a@\n%a@]" binding' b bindings' bs

and bindings'           fmt = MetaPrint.on fmt bindings 
and binding'            fmt = MetaPrint.on fmt binding 
and expression'         fmt = MetaPrint.on fmt expression 
and ty'                 fmt = MetaPrint.on fmt ty 
and type_identifier'    fmt = MetaPrint.on fmt type_identifier 
and identifier'         fmt = MetaPrint.on fmt identifier 
and typing_environment' fmt = MetaPrint.on fmt typing_environment 
and declarations'       fmt = MetaPrint.on fmt declarations 
and declaration'        fmt = MetaPrint.on fmt declaration 




