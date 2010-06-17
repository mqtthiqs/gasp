open StlcdecAST

(* In the future, this module will be automatically generated from the
   programming language meta-theory definition. *)

let ( !+ ) = Name.unique_from_string

(** The following declarations provide constants for concrete
    and internal identifiers. This part should be automatically
    generated in the future. *)

let type_identifier_cname    = "type_identifier"
let type_identifier_iname    = !+ type_identifier_cname

let identifier_cname	     = "identifier"
let identifier_iname	     = !+ identifier_cname

let declaration_cname	     = "declaration"
let declaration_iname	     = !+ declaration_cname

let expression_cname	     = "expression"
let expression_iname	     = !+ expression_cname

let declarations_cname       = "declarations"
let declarations_iname       = !+ declarations_cname

let ty_cname	             = "ty"
let ty_iname		     = !+ ty_cname

let environment_cname        = "environment"
let environment_iname        = !+ environment_cname

let binding_cname	     = "binding"
let binding_iname	     = !+ binding_cname

let fragment_cname	     = "fragment"
let fragment_iname	     = !+ fragment_cname

let typingj_cname	     = "expression_typing_judgment"
let typingj_iname	     = !+ typingj_cname

let modulej_cname	     = "module_typing_judgment"
let modulej_iname	     = !+ modulej_cname

let dvalue_declaration_cname = "DValue"
let dvalue_declaration_iname = !+ dvalue_declaration_cname
let dtype_declaration_cname  = "DType"
let dtype_declaration_iname  = !+ dtype_declaration_cname

let cons_declarations_cname  = "ConsDeclaration"
let cons_declarations_iname  = !+ cons_declarations_cname
let empty_declarations_cname = "EmptyDeclarations"
let empty_declarations_iname = !+ empty_declarations_cname

let var_exp_cname	     = "Var"
let var_exp_iname            = !+ var_exp_cname
let lam_exp_cname	     = "Lam"
let lam_exp_iname	     = !+ lam_exp_cname
let app_exp_cname	     = "App"
let app_exp_iname	     = !+ app_exp_cname

let var_ty_cname	     = "TyVar"
let var_ty_iname	     = !+ var_ty_cname
let arrow_ty_cname	     = "TyArrow"
let arrow_ty_iname	     = !+ arrow_ty_cname

let nil_environment_cname    = "NoBinding"
let nil_environment_iname    = !+ nil_environment_cname
let cons_environment_cname   = "ConsBinding"
let cons_environment_iname   = !+ cons_environment_cname

let fragment_ctor_cname      = "Fragment"
let fragment_ctor_iname      = !+ fragment_ctor_cname

let bind_var_cname	     = "BindVar"
let bind_var_iname	     = !+ bind_var_cname
let bind_tyvar_cname         = "BindTyVar"
let bind_tyvar_iname	     = !+ bind_tyvar_cname

let exp_typingj_cname        = "HasType"
let exp_typingj_iname        = !+ exp_typingj_cname

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
(fragment		    : Type).

(DValue          : identifier -> ty -> expression -> declaration).
(DType	         : type_identifier -> declaration).

(Var             : identifier -> expression).
(Lam             : identifier -> ty -> expression -> expression).
(App             : expression -> expression -> expression).

(TyVar		 : type_identifier -> ty).
(TyArrow	 : ty -> ty -> ty).

(ConsDeclaration   : declaration -> declarations -> declarations).
(EmptyDeclarations : declarations).

(ConsBinding	 : binding -> environment -> environment).
(NoBinding	 : environment).

(Fragment	 : environment -> declarations -> fragment).

(BindVar	 : identifier -> ty -> binding).
(BindTyVar	 : type_identifier -> binding).

(HasType	 : environment -> expression -> ty -> expression_typing_judgment).

(Module	         : environment -> declarations -> environment).

(...)
"

let AST.Patch internalized_prelude =
  ASTparser.patch_from_string prelude

let on_names f l g = 
  let defs = List.map f l in
  let idefs = List.map (fun l -> List.hd (List.rev l)) defs in
  let names = fst (List.split idefs) in 
  List.flatten defs @ g names

let on_name f x g = 
  on_names f [x] (function [y] -> g y | _ -> assert false)

let on_2_names f x1 x2 g = 
  on_names f [x1; x2] (function [y1; y2] -> g y1 y2 | _ -> assert false)

let wr = Position.unknown_pos

let var x = wr (AST.Var x)

let ty_var x = AST.Term (var x)

let app f xs = 
  wr (List.fold_left (fun accu x -> (AST.App (wr accu, x))) (AST.Var f) xs)

let name (ty, t) = (Name.fresh "_", (ty, Some t))

(* FIXME: escape [ and ]. *)
let name_literal lit = Name.from_string (Printf.sprintf "data[%s]" lit)

let rec declaration = function
  | DValue (x, t, e) -> 
      on_name expression' e 
	(fun e_name -> 
	   on_name ty' t 
	     (fun t_name -> 
		on_name identifier' x 
		  (fun x_name -> 
		     [name (ty_var declaration_iname, 
			    app dvalue_declaration_iname 
			      [ x_name; t_name; e_name; ])])))
		       
  | DType x -> 
      on_name type_identifier' x 
	(fun x_name ->
	   [name (ty_var declaration_iname, 
		  app dtype_declaration_iname [ x_name ])])
		    

and declarations = function 
  | EmptyDeclarations -> 
      [name (ty_var declarations_iname, var empty_declarations_iname)]
  | ConsDeclaration (x, xs) -> 
      on_name declaration' x 
	(fun x_name -> 
	   on_name declarations' xs 
	     (fun xs_name -> 
		[name (ty_var declarations_iname, 
		       app cons_declarations_iname [ x_name; xs_name ])]))

and expression = function
  | Var x -> 
      on_name identifier' x 
	(fun x_name ->
	   [name (ty_var expression_iname,
		  app var_exp_iname [ x_name ])])
  | Lam (x, t, e) -> 
      on_name identifier' x 
	(fun x_name ->
	   on_name ty' t
	     (fun t_name ->
		on_name expression' e 
		  (fun e_name -> 
		     [name (ty_var expression_iname,
			    app lam_exp_iname [ x_name; t_name; e_name ])])))
  | App (e1, e2) -> 
      on_2_names expression' e1 e2
	(fun e1_name e2_name ->
	   [name (ty_var expression_iname,
		  app app_exp_iname [ e1_name; e2_name ])])

and ty = function
  | TyVar x ->  
      on_name type_identifier' x 
	(fun x_name ->
	   [name (ty_var ty_iname,
		  app var_ty_iname [ x_name ])])

  | TyArrow (ty1, ty2) -> 
      on_2_names ty' ty1 ty2 
	(fun ty1_name ty2_name ->
	   [name (ty_var ty_iname,
		  app arrow_ty_iname [ ty1_name; ty2_name ])])

and identifier x = 
    [ name_literal x, (ty_var identifier_iname, None) ]

and type_identifier x = 
    [ name_literal x, (ty_var type_identifier_iname, None) ]

and typing_environment (Env bs) = 
  bindings' bs

and bindings = function
  | NoBinding -> 
      [name (ty_var environment_iname, var nil_environment_iname)]
  | ConsBinding (x, xs) -> 
      on_name binding' x 
	(fun x_name -> 
	   on_name bindings' xs 
	     (fun xs_name -> 
		[name (ty_var environment_iname,
		       app cons_environment_iname [ x_name; xs_name ])]))
	
and binding = function
  | BindVar (x, t) -> 
      on_name identifier' x 
	(fun x_name -> 
	   on_name ty' t 
	     (fun t_name ->
		[name (ty_var binding_iname, 
		       app bind_var_iname [ x_name; t_name ])]))
  | BindTyVar x -> 
      on_name type_identifier' x
	(fun x_name ->
	   [name (ty_var binding_iname,
		  app bind_tyvar_iname [ x_name ])])

and bindings'           x = MetaInternalize.on bindings x
and binding'            x = MetaInternalize.on binding x
and expression'         x = MetaInternalize.on expression x
and ty'                 x = MetaInternalize.on ty x
and type_identifier'    x = MetaInternalize.on type_identifier x
and identifier'         x = MetaInternalize.on identifier x
and typing_environment' x = MetaInternalize.on typing_environment x
and declarations'       x = MetaInternalize.on declarations x
and declaration'        x = MetaInternalize.on declaration x

let fragment_view (Fragment (env, decs)) = 
  on_name typing_environment' env 
    (fun env_name ->
       on_name declarations' decs
	 (fun decs_name ->
	    [name (ty_var fragment_iname,
		   app fragment_ctor_iname [ env_name; decs_name ])]))
  
(* precondition: binders > []. *)
let as_ptype iname binders = 
  let cache = Hashtbl.create 13 in
  let binder x b = 
    (* Literal may have been introduced several times. We enforce the invariant
       that bound names are distinct. *)
    if Hashtbl.mem cache x then fun x -> x
    else begin 
      Hashtbl.add cache x (); 
      match snd b with
	| None -> (fun e -> wr (AST.Prod (x, wr (fst b), e)))
	| Some t -> (fun e -> wr (AST.SProd (x, wr (fst b), t, e)))
    end
  in
  let rec aux = function
    | []           -> assert false
    | [(x, b)]     -> binder x b ((binder iname (fst b, Some (var x))) (wr AST.Cont))
    | (x, b) :: bs -> binder x b (aux bs)
  in
  aux binders

let named_fragment_view_from_file name filename = 
  let _fragment_ast : StlcdecAST.fragment = 
    SyntacticAnalysis.parse_file filename StlcdecParser.fragment StlcdecLexer.main
  in
  let fragment_binders = fragment_view _fragment_ast in
  let iname = Name.fresh name in 
  (iname, AST.Patch (as_ptype iname fragment_binders))
  

