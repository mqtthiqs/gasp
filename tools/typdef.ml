module Id = struct
  let name = "pa_modof"
  let version = "1.0"
end

open Camlp4

module Make (Syntax : Sig.Camlp4Syntax) = struct

  open Sig
  include Syntax

  exception Unimplemented

  type modtype_rewrite =
    | MAnd of modtype_rewrite * modtype_rewrite
    | MType of Ast.ctyp * Ast.ctyp
    | MModule of Ast.ident * Ast.module_expr

  module Ident = struct
    type t = Ast.ident
    let rec compare x y = match x,y with
      | <:ident< $lid:i1$ >>, <:ident< $lid:i2$ >>
      | <:ident< $anti:i1$ >>, <:ident< $anti:i2$ >>
      | <:ident< $uid:i1$ >>, <:ident< $uid:i2$ >> ->
	String.compare i1 i2
      | <:ident< $a1$ . $b1$ >>, <:ident< $a2$ . $b2$ >>
      | <:ident< $a1$ $b1$ >>, <:ident< $a2$ $b2$ >> ->
	compare a1 a2 * compare b1 b2
      | <:ident< $list:i1$ >>, <:ident< $list:i2$ >> ->
	Pervasives.compare i1 i2	(* TODO what's going on here?? *)

    open Format
    let rec print fmt = function
      | <:ident< $lid:s$ >> -> printf "%s" s
      | <:ident< $uid:s$ >> -> printf "%s" s
      | <:ident< $anti:s$ >> -> printf "'%s" s
      | <:ident< $a$ . $b$ >> -> printf "%a.%a" print a print b
      | <:ident< $a$ $b$ >> -> printf "%a %a" print a print b
  end

  module Sigma = struct
    module Identmap = Map.Make(Ident)
    type t = Ast.ctyp Identmap.t * Ast.module_expr Identmap.t
    let add_typ x t (tymap, modmap) = Format.eprintf "add_typ %a\n" Ident.print x; Identmap.add x t tymap, modmap
    let add_mod x t (tymap, modmap) = Format.eprintf "add_mod %a\n" Ident.print x; tymap, Identmap.add x t modmap
    let find_typ x (tymap, modmap) = Format.eprintf "find_typ %a\n" Ident.print x; Identmap.find x tymap
    let find_mod x (tymap, modmap) = Format.eprintf "find_mod %a\n" Ident.print x; Identmap.find x modmap
    let empty = Identmap.empty, Identmap.empty
    let cardinal (tymap, modmap) = Identmap.fold (fun _ _ n -> n + 1) tymap 
      (Identmap.fold (fun _ _ n -> n + 1) modmap 0)
  end

  let rec build_rewrites sigma = function
    | MAnd (w1, w2) -> build_rewrites (build_rewrites sigma w2) w1
    (* TODO: as in (see below): need to find a solution for general type parameters *)
    | MType(<:ctyp<$id:id$ '$_$>>,td)
    | MType(<:ctyp<$id:id$ '$_$ '$_$>>,td)
    | MType(<:ctyp<$id:id$ '$_$ '$_$ '$_$>>,td)
    | MType(<:ctyp<$id:id$ '$_$ '$_$ '$_$ '$_$>>,td)
    | MType(<:ctyp<$id:id$>>, td) ->
      Sigma.add_typ id td sigma

    | MType(<:ctyp<$_$>>, _) -> raise Unimplemented (* TODO location *)

    | MModule(id, me) ->
      Sigma.add_mod id me sigma

  let rec rewrite_type path sigma : Ast.ctyp -> Ast.ctyp = Printf.eprintf "coucou\n"; function
    | <:ctyp@loc< $lid:id$ = $t$ >> ->
      Printf.eprintf "#### on cherche %s\n" id;
      begin 
	try <:ctyp@loc< $lid:id$ == $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> 
	  Printf.eprintf "#### pas trouvé\n";	  
	  <:ctyp@loc< $lid:id$ == $t$ >>
      end

    (* TODO: find a remedy for that: we need to repeat type declarations with 
       1, 2, 3... parameters since there is no quotations for the n case *)
    | <:ctyp@loc< $lid:id$ $p$ == $t$ >> ->
      Printf.eprintf "#### on cherche %s\n" id;
      begin
    	try <:ctyp@loc< $lid:id$ $p$ == $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> 
	  Printf.eprintf "#### pas trouvé\n";
	  <:ctyp@loc< $lid:id$ $p$ == $t$ >>
      end
    | <:ctyp@loc< $lid:id$ $p$ $q$ == $t$ >> ->
      begin
    	try <:ctyp@loc< $lid:id$ $p$ $q$ == $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> <:ctyp@loc< $lid:id$ $p$ $q$ == $t$ >>
      end
    | <:ctyp@loc< $lid:id$ $p$ $q$ $r$ == $t$ >> ->
      begin
    	try <:ctyp@loc< $lid:id$ $p$ $q$ $r$ == $Sigma.find_typ (path id) sigma$ >>
        with Not_found -> <:ctyp@loc< $lid:id$ $p$ $q$ $r$ == $t$ >>
      end
    (* end of TODO *)

    | <:ctyp@loc< $typ:t1$ and $typ:t2$ >> ->
      <:ctyp@loc< $rewrite_type path sigma t1$ and $rewrite_type path sigma t2$ >>

    | t -> t				(* TODO why do we need that? *)

  and rewrite_sig_item path sigma : Ast.sig_item -> Ast.str_item = function
    | <:sig_item@loc< $s1$ ; $s2$ >> ->
      <:str_item@loc< $rewrite_sig_item path sigma s1$; $rewrite_sig_item path sigma s2$ >>
    | <:sig_item@loc< open $os$ >> -> <:str_item@loc< open $os$ >>
    | <:sig_item@loc< module $id$ : $mt$>> ->
      begin
	try <:str_item@loc< module $id$ = $Sigma.find_mod (path id) sigma$ >>
        with Not_found ->
	  let path y = <:ident@here< $path id$.$lid:y$ >> in
	  <:str_item@loc< module $id$ = $rewrite_modtype path sigma mt$ >>
      end

    | <:sig_item@loc< type $t$ >> -> <:str_item@loc< type $rewrite_type path sigma t$ >>


    | <:sig_item@loc< module type $id$ = $mt$ >> -> <:str_item@loc< module type $id$ = $mt$ >>
    | <:sig_item@loc< exception $ts$ >> -> <:str_item@loc< exception $ts$ >>
    | <:sig_item@loc< include $mt$ >> -> 
      <:str_item@loc< include $rewrite_modtype path sigma mt$ >>

    | <:sig_item@loc< module rec $_$ >>
    | <:sig_item@loc< class $_$ >>
    | <:sig_item@loc< $anti:_$ >>
    | <:sig_item@loc< external $_$ : $_$ = $_$ >>
    | <:sig_item@loc< class type $_$ >> -> Loc.raise loc Unimplemented

    | <:sig_item@loc< # $_$ >>
    | <:sig_item@loc< # $_$ $_$ >>
    | <:sig_item@loc< >>
    | <:sig_item@loc< value $_$ : $_$ >> -> <:str_item@loc< >>

  and rewrite_modtype path sigma : Ast.module_type -> Ast.module_expr = function
    | <:module_type@loc< sig $si$ end >> ->
      <:module_expr@loc< struct $rewrite_sig_item path sigma si$ end>>
    | <:module_type@loc< functor ($argn$ : $argt$) -> $mt$ >> ->
      <:module_expr@loc< functor ($argn$ : $argt$) -> $rewrite_modtype path sigma mt$ >>
    | <:module_type@loc< $id:id$ >> -> Loc.raise loc Unimplemented
    | <:module_type@loc< >> -> assert false
    | <:module_type@loc< $_$ with $_$ >>
    | <:module_type@loc< $anti:_$ >>
    | <:module_type@loc< '$_$ >> -> Loc.raise loc Unimplemented (* TODO complete *)

  let modtype_rewrite = Gram.Entry.mk "modtype_rewrite"

  EXTEND Gram
    modtype_rewrite:
      [[ e1 = SELF; LIDENT "and"; e2 = SELF -> MAnd(e1,e2)
       | "type"; t1 = ctyp; "="; t2 = ctyp -> MType(t1,t2)
       | "module"; id = ident; "="; me = module_expr -> MModule(id,me)
      ]];

    module_type:
      [[ LIDENT "mli" ->
	begin
	  try let nmli = (Filename.chop_extension (!Camlp4_config.current_input_file))^".mli" in
	    if nmli = !Camlp4_config.current_input_file then <:module_type<sig end>> else
	      let mli = Syntax.parse_interf (Loc.mk nmli) (Stream.of_channel (open_in (nmli))) in
	      <:module_type<sig $mli$ end>>
	  with Sys_error _ -> <:module_type<sig end>>
	end
       ]];

    module_expr:
      [ [
	"module"; "of"; e = module_type ->
	rewrite_modtype (fun x -> <:ident< $lid:x$ >>) Sigma.empty e
      |	"module"; "of"; e = module_type; "using"; r = modtype_rewrite ->
	rewrite_modtype (fun x -> <:ident< $lid:x$ >>) (build_rewrites Sigma.empty r) e
      ] ];
  END
  ;;

  ErrorHandler.register
    (fun ppf -> function 
      | Unimplemented ->
        Format.fprintf ppf "%s: translation unimplemented for that kind of sig_item" Id.name
      | exn -> raise exn)
  ;;

end

let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
