module Id = struct
  let name = "pa_typof"
  let version = "1.0"
end

open Camlp4

module Make (Syntax : Sig.Camlp4Syntax) = struct

  open Sig
  include Syntax

  exception Unimplemented

  module Ident = struct
    type t = Ast.ident
    let rec compare x y = match x,y with
      | <:ident< $lid:i1$ >>, <:ident< $lid:i2$ >>
      | <:ident< $anti:i1$ >>, <:ident< $anti:i2$ >>
      | <:ident< $uid:i1$ >>, <:ident< $uid:i2$ >> ->
	Printf.eprintf "compare %s <> %s\n" i1 i2;
	String.compare i1 i2
      | <:ident< $a1$ . $b1$ >>, <:ident< $a2$ . $b2$ >>
      | <:ident< $a1$ $b1$ >>, <:ident< $a2$ $b2$ >> ->
	(* compare a1 a2 * compare b1 b2 *)
	assert false
      | <:ident< $list:i1$ >>, <:ident< $list:i2$ >> ->
	(* compare i1 i2 *)
	assert false
  end

  module Sigma = struct
    module Identmap = Map.Make(Ident)
    type t = Ast.ctyp Identmap.t * Ast.ident Identmap.t
    let add_typ x t (tymap, modmap) = Identmap.add x t tymap, modmap
    let add_mod x t (tymap, modmap) = tymap, Identmap.add x t modmap
    let find_typ x (tymap, modmap) = Identmap.find x tymap
    let find_mod x (tymap, modmap) = Identmap.find x modmap
    let empty = Identmap.empty, Identmap.empty
    let cardinal (tymap, modmap) = Identmap.fold (fun _ _ n -> n + 1) tymap 
      (Identmap.fold (fun _ _ n -> n + 1) modmap 0)
  end

  let rec add_with_constr sigma = function
    | <:with_constr< $w1$ and $w2$ >> ->
      add_with_constr (add_with_constr sigma w2) w1
    (* TODO: as in (see below): need to find a solution for general type parameters *)
    | <:with_constr< type $id:id$ '$_$ = $td$ >>
    | <:with_constr< type $id:id$ '$_$ '$_$ = $td$ >>
    | <:with_constr< type $id:id$ '$_$ '$_$ '$_$ = $td$ >>
    | <:with_constr< type $id:id$ '$_$ '$_$ '$_$ '$_$ = $td$ >>
    | <:with_constr< type $id:id$ '$_$ '$_$ '$_$ '$_$ '$_$ = $td$ >>
    | <:with_constr< type $id:id$ = $td$ >> ->
      Sigma.add_typ id td sigma

    | <:with_constr@loc< type $_$ = $_$ >> -> raise (Loc.Exc_located (loc, Unimplemented)) (* TODO *)

    | <:with_constr< module $id1$ = $id2$ >> ->
      Sigma.add_mod id1 id2 sigma
    | <:with_constr< >> -> sigma
    | <:with_constr@loc< $anti:_$ >> -> raise (Loc.Exc_located (loc, Unimplemented))

  let rec rewrite_sig_item path sigma : Ast.sig_item -> Ast.str_item = function
    | <:sig_item@loc< $s1$ ; $s2$ >> ->
      <:str_item@loc< $rewrite_sig_item path sigma s1$; $rewrite_sig_item path sigma s2$ >>
    | <:sig_item@loc< open $os$ >> -> <:str_item@loc< open $os$ >>
    | <:sig_item@loc< module $id$ : $mt$>> ->
      begin
	try <:str_item@loc< module $id$ = $id:Sigma.find_mod (path id) sigma$ >>
        with Not_found ->
	  let path y = <:ident@here< $path id$.$lid:y$ >> in
	  <:str_item@loc< module $id$ = $rewrite_modtype path sigma mt$ >>
      end
    | <:sig_item@loc< type $lid:id$ = $typ:t$ >> ->
      Printf.eprintf "#### on cherche %s\n" id;
      begin 
	try <:str_item@loc< type $lid:id$ = $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> 
	  Printf.eprintf "#### pas trouvé\n";	  
	  <:str_item@loc< type $lid:id$ = $t$ >>
      end

    (* TODO: find a remedy for that: we need to repeat type declarations with 
       1, 2, 3... parameters since there is no quotations for the n case *)
    | <:sig_item@loc< type $lid:id$ $p$ = $t$ >> ->
      Printf.eprintf "#### on cherche %s\n" id;
      begin
    	try <:str_item@loc< type $lid:id$ $p$ = $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> 
	  Printf.eprintf "#### pas trouvé\n";
	  <:str_item@loc< type $lid:id$ $p$ = $t$ >>
      end
    | <:sig_item@loc< type $lid:id$ $p$ $q$ = $t$ >> ->
      begin
    	try <:str_item@loc< type $lid:id$ $p$ $q$ = $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> <:str_item@loc< type $lid:id$ $p$ $q$ = $t$ >>
      end
    | <:sig_item@loc< type $lid:id$ $p$ $q$ $r$ = $t$ >> ->
      begin
    	try <:str_item@loc< type $lid:id$ $p$ $q$ $r$ = $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> <:str_item@loc< type $lid:id$ $p$ $q$ $r$ = $t$ >>
      end
    (* end of TODO *)

    | <:sig_item@loc< type $_$ >> -> raise (Loc.Exc_located (loc, Unimplemented)) (* TODO *)
    | <:sig_item@loc< module type $id$ = $mt$ >> -> <:str_item@loc< module type $id$ = $mt$ >>
    | <:sig_item@loc< exception $ts$ >> -> <:str_item@loc< exception $ts$ >>
    | <:sig_item@loc< include $mt$ >> -> 
      <:str_item@loc< include $rewrite_modtype path sigma mt$ >>

    | <:sig_item@loc< module rec $_$ >>
    | <:sig_item@loc< class $_$ >>
    | <:sig_item@loc< $anti:_$ >>
    | <:sig_item@loc< external $_$ : $_$ = $_$ >>
    | <:sig_item@loc< class type $_$ >> -> raise (Loc.Exc_located (loc, Unimplemented))

    | <:sig_item@loc< # $_$ >>
    | <:sig_item@loc< # $_$ $_$ >>
    | <:sig_item@loc< >>
    | <:sig_item@loc< value $_$ : $_$ >> -> <:str_item@loc< >>

  and rewrite_modtype path sigma : Ast.module_type -> Ast.module_expr = function
    | <:module_type@loc< sig $si$ end >> ->
      <:module_expr@loc< struct $rewrite_sig_item path sigma si$ end>>
    | <:module_type@loc< $mt$ with $wc$ >> ->
      let sigma = add_with_constr sigma wc in
      Printf.eprintf "sigma contient %d bindings \n" (Sigma.cardinal sigma);
      rewrite_modtype path (sigma) mt
    | <:module_type@loc< functor ($argn$ : $argt$) -> $mt$ >> ->
      <:module_expr@loc< functor ($argn$ : $argt$) -> $rewrite_modtype path sigma mt$ >>
    | <:module_type@loc< $id:id$ >> -> 
      raise (Loc.Exc_located (loc, Unimplemented))
    | <:module_type@loc< >> -> assert false
    | _ -> raise (Loc.Exc_located (Loc.ghost, Unimplemented)) (* TODO complete *)

  EXTEND Gram
    module_type:
    [[ LIDENT "mli" ->
      try
	let nmli = (Filename.chop_extension (!Camlp4_config.current_input_file))^".mli" in
	if nmli = !Camlp4_config.current_input_file then <:module_type<sig end>> else
	  let mli = Syntax.parse_interf (Loc.mk nmli) (Stream.of_channel (open_in (nmli))) in
	  <:module_type<sig $mli$ end>>
      with Sys_error _ -> <:module_type<sig end>>
    ]];

    module_expr:
      [[ LIDENT "types"; "of"; e = module_type ->
      rewrite_modtype (fun x -> <:ident< $lid:x$ >>) Sigma.empty e
      ]];
  END;;

    ErrorHandler.register
    (fun ppf -> function 
      | Unimplemented ->
        Format.fprintf ppf "%s: translation unimplemented for that kind of sig_item" Id.name
      | exn -> raise exn)
end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
