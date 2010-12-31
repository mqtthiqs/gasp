open NLF
open NLF

exception Not_convertible_fam of fam * fam
exception Not_convertible_obj of obj * obj

let rec conv_args' ea eb =
  NLFEnv.fold 
    (fun x e () ->
       try
	 match NLFEnv.find eb x, e with
	   | NLFEnv.ODecl a, NLFEnv.ODecl b ->
	       (* conv_fam a b *) assert false
	   | NLFEnv.ODef (Obj(tenv,ht,ha)), NLFEnv.ODef (Obj(uenv,hu,hb)) ->
	       (* (try match NLFEnv.find (NLFEnv.merge tenv eb) "n" with NLFEnv.ODecl a ->  *)
	       (* 	  Format.printf "dans tenv, n : %a@," NLF_pp.fam a *)
	       (* 	  | NLFEnv.ODef t -> Format.printf "dans tenv, n = %a@," NLF_pp.obj t  *)
	       (* 	with Not_found -> Format.printf"pas de n@,"); *)
	       conv_obj 
		 (Obj(NLFEnv.merge tenv (NLFEnv.clear eb), ht, ha)) 
		 (Obj(NLFEnv.merge uenv (NLFEnv.clear ea), hu, hb))
	   | NLFEnv.ODecl a, NLFEnv.ODef t
	   | NLFEnv.ODef t, NLFEnv.ODecl a ->
	       assert false
       with Not_found -> Format.printf "@.Error: %s not found in %a@," x NLF_pp.env eb; exit 1
    ) ea ();

and conv_args ea eb = 
  Format.printf "@[<v 2>* conv_args @[%a@] == @[%a@] {@," NLF_pp.env ea NLF_pp.env eb;
  conv_args' ea eb;
  Format.printf"@]}@,"

and conv_obj' (Obj(envt, ht, _) as t) (Obj(envu, hu, _) as u) =
  match ht, hu with
    | OConst(c,at), OConst(d,au) ->
	if c = d then conv_args at au
	else raise (Not_convertible_obj(t,u))
    | OConst(c,ac), OVar(x,ax) ->
	begin match NLFEnv.find envu x with
	  | NLFEnv.ODecl a -> raise (Not_convertible_obj(t,u))
	  | NLFEnv.ODef t' -> conv_obj t t'	(* TODO et ac et ax?? *)
	end
    | OVar(x,ax), OConst(c,ac) ->
	begin match NLFEnv.find envt x with
	  | NLFEnv.ODecl a -> raise (Not_convertible_obj(t,u))
	  | NLFEnv.ODef t' -> conv_obj t' t	(* TODO et ac et ax?? *)
	end
    | OVar(x,ax), OVar(y,ay) ->
	if x = y then conv_args (NLFEnv.merge envt ax) (NLFEnv.merge envu ay)
	else
	  begin match NLFEnv.find envt x, NLFEnv.find envu y with
	    | NLFEnv.ODecl _, NLFEnv.ODecl _ -> raise (Not_convertible_obj(t,u))
	    | NLFEnv.ODecl _, NLFEnv.ODef _ 
	    | NLFEnv.ODef _, NLFEnv.ODecl _ -> assert false
	    | NLFEnv.ODef _, NLFEnv.ODef _ -> assert false
	  end
    | _ -> assert false

and conv_obj t u =
  Format.printf "@[<v 2>* conv_obj @[%a@] == @[%a@]{@," NLF_pp.obj t NLF_pp.obj u;
  conv_obj' t u;
  Format.printf"@]}@,"
	
and conv_fam' a b =
  match a, b with
    | Fam(enva,FConst(ca,argsa)), Fam(envb,FConst(cb,argsb)) when ca=cb ->
	conv_args
	  (NLFEnv.merge (NLFEnv.clear enva) argsa) 
	  (NLFEnv.merge (NLFEnv.clear envb) argsb)
    | _ -> raise (Not_convertible_fam(a,b))

and conv_fam t u =
  Format.printf "@[<v 2>* conv_fam @[%a@] == @[%a@]{@," NLF_pp.fam t NLF_pp.fam u;
  conv_fam' t u;
  Format.printf"@]}@,"

let rec args' sign e1 e2 =
  NLFEnv.fold
    (fun x e () ->
       match NLFEnv.find e1 x, e with
	 | NLFEnv.ODecl a, NLFEnv.ODecl b -> 
	     conv_fam a b;
	     Format.printf "Argument ODecl: %s : %a@," x NLF_pp.fam a;
	 | NLFEnv.ODef (Obj(tenv, ht, ha)), NLFEnv.ODecl (Fam(aenv, ha')) ->
	     conv_fam (Fam(tenv, ha)) (Fam(aenv, ha'));
	     obj sign (Obj(NLFEnv.merge tenv aenv, ht,ha'));
	 | _ -> assert false
    ) e2 ()

and args sign env e1 e2 =
  Format.printf "@[<v 2>* args @[%a@] :: @[%a@]{@," NLF_pp.env e1 NLF_pp.env e2;
  args' sign
    (NLFEnv.merge (NLFEnv.clear env) e1)
    (NLFEnv.merge (NLFEnv.clear env) e2);
  Format.printf "@]}@,"

and obj' sign : obj -> unit = function
  | Obj(env, ht, ha) -> match ht with
      | OConst(c, cargs) ->
	  begin match NLFSign.find sign c with
	    | NLFSign.FDecl k -> 
		Format.printf "Argument FDecl: %s : %a@," c NLF_pp.kind k;
		assert false       (* cst de fam dans un obj *)
	    | NLFSign.ODecl (Fam(cenv, hc)) ->
		args sign env cargs cenv;
		conv_fam (Fam(cargs,ha)) (Fam(cargs,hc))
	  end
      | OVar(x, xargs) -> 
	  begin match NLFEnv.find env x with
	    | NLFEnv.ODecl (Fam(xenv, hc)) -> 
		args sign env xargs xenv;
		conv_fam (Fam(xargs,ha)) (Fam(xenv,hc))
	    | NLFEnv.ODef (Obj(xenv, hc, hac)) ->
		obj sign (Obj(xargs, hc, hac));
		args sign env xargs xenv;
		conv_fam (Fam(xargs,hac)) (Fam(env,ha))
	  end
      | OApp(Obj(tenv,ht,ha'), targs) -> 
	  obj sign (Obj (targs, ht, ha'));
	  args sign env targs tenv;
	  conv_fam (Fam(targs, ha')) (Fam(tenv, ha))
	    
and obj sign t = 
  Format.printf "@[<v 2>* obj @[%a@]{@," NLF_pp.obj t;
  obj' sign t;
  Format.printf "@]}@,"

let fam' sign : fam -> unit = function
  | Fam (env, FConst(c, cargs)) ->
      begin match NLFSign.find sign c with
	| NLFSign.ODecl _ -> assert false	(* cst d'objet ds une fam  *)
	| NLFSign.FDecl (Kind cenv) -> 
	    args sign env cargs cenv
      end

let fam sign a = 
  Format.printf "@[<v 2>* fam @[%a@]{@," NLF_pp.fam a;
  fam' sign a;
  Format.printf "@]}@,"

let kind' sign : kind -> unit = function
  | Kind env ->
      NLFEnv.fold
	(fun x e () -> 
	   match e with
	     | NLFEnv.ODecl a -> fam sign a
	     | NLFEnv.ODef t -> obj sign t
	) env ()

let kind sign k =
  Format.printf "@[<v 2>* kind @[%a@]{@," NLF_pp.kind k;
  kind' sign k;
  Format.printf "@]}@,"

let sign s = 
  ignore(NLFSign.fold 
	   (fun c e sign ->
	      match e with
		| NLFSign.FDecl k -> 
		    kind sign k; sign
		| NLFSign.ODecl a -> 
		    fam sign a; sign
	   ) s s)
