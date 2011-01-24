#load "pa_extend.cmo";;
#load "q_MLast.cmo";;
#load "pa_macro.cmo";;

open MLast
open Pcaml

let rec list_last = function
  | [] -> raise (Invalid_argument "list_last")
  | [x] -> x
  | x :: xs -> list_last xs

let rec with_constr_td_assoc path : with_constr list -> type_decl = function
  | [] -> raise Not_found
  | WcTyp(loc, p, tv, b, t) :: wcs -> 
      if path=p then {tdNam=loc, list_last p; tdPrm=tv; tdPrv=b; tdDef=t; tdCon=[]}
      else with_constr_td_assoc path wcs
  | WcMod(loc, p, me) :: wcs -> with_constr_td_assoc path wcs

let rec with_constr_mod_assoc path : with_constr list -> module_expr = function
  | [] -> raise Not_found
  | WcTyp _ :: wcs -> with_constr_mod_assoc path wcs
  | WcMod(loc, p, me) :: wcs ->
      if path=p then me
      else with_constr_mod_assoc path wcs
	
let subst_tds path sigma : type_decl list -> type_decl list =
  List.map
    (fun td ->
       try with_constr_td_assoc (List.rev (snd td.tdNam :: path)) sigma
       with Not_found -> td
    )

let rec subst_mts path sigma r
    : (string * module_type) list -> (string * module_expr) list =
  List.map
    (fun (x,mt) -> 
       let me = 
	 try with_constr_mod_assoc (x::path) sigma 
	 with Not_found -> types (x::path) sigma mt in
       x, if r then MeTyc(Ploc.dummy, me, mt) else me
    )

and types path sigma : module_type -> module_expr = function
  | MtSig(loc, items) -> 
      let l = List.fold_right
	(fun si acc ->
	   match si with
	     | SgTyp(loc, tds) -> 
		 StTyp(loc, subst_tds path sigma tds) :: acc
	     | SgMod(loc, r, mts) ->
		 StMod(loc, r, subst_mts path sigma r mts) :: acc
	     | SgExc(loc, n, ts) -> StExc(loc, n, ts, []) :: acc
	     | _ -> acc
	) items [] in
      MeStr(loc, l)
  | MtWit(_, mt, wcs) -> types path (wcs @ sigma) mt
  | _ -> raise (Invalid_argument "types")

EXTEND
module_type:
  [[ LIDENT "mli" -> 
       try
	 let nmli = (Filename.chop_extension
		       (!Pcaml.input_file))^".mli" in
	 if nmli = !Pcaml.input_file then <:module_type<sig end>> else
	   let (mli,_) = !Pcaml.parse_interf (Stream.of_channel (open_in (nmli))) in
	   MtSig(Ploc.dummy, List.map fst mli)
       with Sys_error _ -> <:module_type<sig end>>
   ]];

module_expr:
  [[ LIDENT "types"; "of"; e = module_type -> 
       types [] [] e
  ]];
END

