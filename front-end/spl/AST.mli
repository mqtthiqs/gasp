(** This module defines the type of Abstract Syntax Trees. *)

(** A program is a list of declarations. *)
type program = declarations

and declarations = declaration list

(** Declarations introduce type identifiers, identifiers, constructor
    identifiers into scope. Each identifier is associated to a
    definition depending on its kind. *)
and declaration = 
  | DType of type_identifier  * constructor_declarations
  | DVal  of binding * expression

and constructor_declarations = constructor_declaration list

and constructor_declaration = constructor_identifier * types 

and function_definition = binding * typ * expression

(** The syntax for types. *)
and typ = 
  | TVar of type_identifier (** User-defined types.    *)
  | TInt                    (** Integers.	       *)
  | TUnit                   (** Unit type.	       *)
  | TArrow of typ * typ     (** Function types.        *)

and types = typ list

and binding = identifier * typ

(** The syntax for exceptions. *)
and expression = 
  | EVar	 of identifier
  | EInt	 of int
  | EUnit	 
  | ECApp	 of constructor_identifier * expressions
  | ESeq	 of expressions
  | EVal	 of binding * expression * expression
  | EMatch	 of expression * branches
  | EApp         of expression * expression
  | EFun         of function_definition
  | EPrim2       of binary_operator
  | EPrim1       of unary_operator

and condition = expression

and branch = pattern * expression

and branches = branch list

and pattern = 
  | PVar of identifier
  | PCon of constructor_identifier * patterns

and patterns = pattern list

and binary_operator =   
  | Add (** Arithmetic operators. *)
  | Mul | Div | Sub | Mod | Pow 
  
  | Gt (** Comparison operators. *)
  | Lt | Le | Ge | Eq | Neq

and unary_operator =
  | Neg  (** Arithmetic operator. *)

and expressions = expression list

and identifier = Identifier of string

and identifiers = identifier list

and constructor_identifier = CIdentifier of string

and type_identifier = TIdentifier of string

type tenv = 
  | BindVal of binding
  | BindTy of type_identifier * constructor_declarations
  | EmptyEnv
  | JoinEnv of tenv * tenv


