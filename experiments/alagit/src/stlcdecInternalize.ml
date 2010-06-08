let ( !+ ) = Name.unique_from_string

(** The following declarations provide constants for concrete
    and internal identifiers. This part should be automatically
    generated in the future. *)
let declaration_cname	     = "declaration"
let declaration_iname	     = !+ declaration_cname
let dvalue_declaration_cname = "DValue"
let dvalue_declaration_iname = !+ dvalue_declaration_cname
let dtype_declaration_cname  = "DType"
let dtype_declaration_iname  = !+ dtype_declaration_cname

let declarations_cname       = "declarations"
let declarations_iname       = !+ declarations_cname
let cons_declarations_cname  = "ConsDeclaration"
let cons_declarations_iname  = !+ cons_declarations_cname
let empty_declarations_cname = "EmptyDeclarations"
let empty_declarations_iname = !+ empty_declarations_cname

let expression_cname	     = "expression"
let expression_iname	     = !+ expression_cname
let var_exp_cname	     = "Var"
let var_exp_iname            = !+ var_exp_cname
let lam_exp_cname	     = "Lam"
let lam_exp_iname	     = !+ lam_exp_cname
let app_exp_cname	     = "App"
let app_exp_iname	     = !+ app_exp_cname

let ty_cname	             = "ty"
let ty_iname		     = !+ ty_cname
let var_ty_cname	     = "TyVar"
let var_ty_iname	     = !+ var_ty_cname
let arrow_ty_cname	     = "TyArrow"
let arrow_ty_iname	     = !+ arrow_ty_cname

let environment_cname        = "environment"
let environment_iname        = !+ environment_cname
let nil_environment_cname    = "NoBinding"
let nil_environment_iname    = !+ nil_environment_cname
let cons_environment_cname   = "ConsBinding"
let cons_environment_iname   = !+ cons_environment_cname

let binding_cname	     = "binding"
let binding_iname	     = !+ binding_cname
let bind_var_cname	     = "BindVar"
let bind_var_iname	     = !+ bind_var_cname
let bind_tyvar_cname         = "BindTyVar"
let bind_tyvar_iname	     = !+ bind_tyvar_cname

let typingj_cname	     = "expression_typing_judgment"
let exp_typingj_cname        = "HasType"
let exp_typingj_iname        = !+ exp_typingj_cname

let modulej_cname	     = "module_typing_judgment"
let module_typingj_cname     = "Module"
let module_typingj_iname     = !+ module_typingj_cname

(** The internalized meta-theory of STLCDEC. *)

let prelude = "
(type_identifier            : Type).
(identifier		    : Type).
(declaration		    : Type).
(expression		    : Type).
(declarations		    : Type).
(ty		            : Type).
(environment		    : Type).
(binding	            : Type).
(expression_typing_judgment : Type).
(module_typing_judgment     : Type).

(DValue          : identifier -> ty -> expression -> declaration).
(DType	         : type_identifier -> declaration).

(Var             : identifier -> expression).
(Lam             : identifier -> ty -> expression -> expression).
(App             : expression -> expression -> expression).

(TyVar		 : identifier -> ty).
(TyArrow	 : ty -> ty -> ty).

(ConsBinding	 : binding -> environment -> environment).
(NoBinding	 : environment).

(BindVar	 : identifier -> ty -> binding).
(BindTyVar	 : identifier -> binding).

(HasType	 : environment -> expression -> ty -> expression_typing_judgment).

(Module	         : environment -> declarations -> environment).
...
"

let AST.Patch internalized_prelude =
  let parser_fun lexer lexbuf = try
    Parser.patch lexer lexbuf
  with Parser.Error -> assert false 
  in
  SyntacticAnalysis.process 
    ~lexer_init:Lexing.from_string 
    ~parser_fun 
    ~lexer_fun:Lexer.main 
    ~input:prelude

let initial_repository = 
  Check.infer_env Env.empty internalized_prelude

let fragment f = 
  assert false
