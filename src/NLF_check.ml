open NLF
open NLF

exception Not_convertible of fam * fam

let rec conv_args ea eb =
  NLFEnv.fold 
    (fun x e () ->
       match NLFEnv.find eb x, e with
	 | NLFEnv.ODecl a, NLFEnv.ODecl b ->
	     conv_fam a b
	 | NLFEnv.ODef t, NLFEnv.ODef u ->
	     conv_obj t u
	 | NLFEnv.ODecl a, NLFEnv.ODef t
	 | NLFEnv.ODef t, NLFEnv.ODecl a ->
	     assert false
    ) ea ()

and conv_obj (Obj(envt, ht, _)) (Obj(envu, hu, _)) =
  match ht, hu with
    | OConst(c,at), OConst(d, au) when c=d ->
	conv_args at au
    | _ -> assert false

and conv_fam a b =
  match a, b with
    | Fam(enva,FConst(ca,argsa)), Fam(envb,FConst(cb,argsb)) when ca=cb ->
	conv_args argsa argsb
    | _ -> raise (Not_convertible(a,b))

let rec args sign env e1 e2 =
  Format.printf"===@.";
  ignore(NLFEnv.fold
	   (fun x e env ->
	      match NLFEnv.find e1 x, e with
		| NLFEnv.ODecl a, NLFEnv.ODecl b -> 
		    conv_fam a b;
		    Format.printf "Argument ODecl: %s : %a@." x NLF_pp.fam a;
		    env
		| NLFEnv.ODef (Obj(tenv, ht, ha)) as t,
		    NLFEnv.ODecl (Fam(aenv, ha')) ->
		    conv_fam (Fam(tenv, ha)) (Fam(aenv, ha'));
		    obj sign (Obj(NLFEnv.merge (NLFEnv.merge env tenv) aenv, ht,ha'));
		    NLFEnv.add env x t
		| _ -> assert false
	   ) e2 env)

and obj sign : obj -> unit = function
  | Obj(env, ht, ha) -> match ht with
      | OConst(c, cargs) ->
	  begin match NLFSign.find sign c with
	    | NLFSign.FDecl _ -> assert false       (* cst de fam dans un obj *)
	    | NLFSign.ODecl (Fam(cenv, hc)) ->
		args sign env cargs cenv;
		conv_fam (Fam(cargs,ha)) (Fam(env,hc))
	  end
      | OVar(x, xargs) -> 
	  begin match NLFEnv.find env x with
	    | NLFEnv.ODecl (Fam(xenv, hc)) -> 
		args sign env xargs xenv;
		conv_fam (Fam(xargs,ha)) (Fam(xenv,hc))
	    | NLFEnv.ODef (Obj(xenv, hc, hac)) ->
		obj sign (Obj(NLFEnv.merge env xargs, hc, hac));
		args sign env xargs xenv;
		conv_fam (Fam(xargs,hac)) (Fam(env,ha))
	  end
      | OApp(t, targs) -> 
	  assert false

let fam sign : fam -> unit = function
  | Fam (env, FConst(c, cargs)) ->
      begin match NLFSign.find sign c with
	| NLFSign.ODecl _ -> assert false	(* cst d'objet ds une fam  *)
	| NLFSign.FDecl (Kind cenv) -> 
	    args sign env cargs cenv
      end

let kind sign : kind -> unit = function
  | Kind env ->
      NLFEnv.fold
	(fun x e () -> 
	   match e with
	     | NLFEnv.ODecl a -> fam sign a
	     | NLFEnv.ODef t -> obj sign t
	) env ()  

let sign s = 
  ignore(NLFSign.fold 
	   (fun c e sign ->
	      match e with
		| NLFSign.FDecl k -> 
		    Format.printf "Check kind @[%s@ :@ %a@]@." c NLF_pp.kind k;
		    kind sign k; sign
		| NLFSign.ODecl a -> 
		    Format.printf "Check fam %s : %a@." c NLF_pp.fam a;
		    fam sign a; sign
	   ) s s)
