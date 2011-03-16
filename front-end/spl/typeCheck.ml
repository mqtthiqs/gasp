open AST
open Signature

let rec program : tenv -> program -> SLF.term * tenv = fun env -> function
  | [] -> 
    (tc_emptyprog env, EmptyEnv)
  | d :: ds -> 
    let hd, env' = declaration env d in
    let htcd = tc_declprog env d env' hd in
    let hds, env''' = program (JoinEnv (env, env')) ds in 
    let env'' = JoinEnv (env, env') in
    let hjoin_subenv = subenv_evidence env'' env'' in
    (tc_joinprog env env' env'' env''' [d] ds htcd hjoin_subenv hds, 
     JoinEnv (env', env''))

and declaration : tenv -> declaration -> SLF.term * tenv = fun env -> function
  | DVal ((x, ty), e) -> 
    (tc_valdecl env x ty e (expression env ty e), BindVal ((x, ty)))
  | _ -> assert false

and expression : tenv -> typ -> expression -> SLF.term = fun env ty -> function
  | EInt x -> 
    check_types ty TInt;
    tc_eint env x
  | _ -> 
    assert false

and check_types ty1 ty2 = 
  if not (ty1 = ty2) then 
    Error.global_error "Type checking" 
      (Printf.sprintf "%s <> %s"
	 (Misc.ExtPprint.as_string (Printer.typ ty1))
	 (Misc.ExtPprint.as_string (Printer.typ ty2)))

let program : program -> SLF.term = 
  fun p -> 
    fst (program EmptyEnv p)
