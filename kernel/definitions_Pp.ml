open Format

module Make (Definitions : Definitions.Sig) = struct

  open Definitions
  module E = Environment_Pp.Make (Definitions)

  let pp_construct pp_ident pp_ty pp_obj pp_term fmt = 
    let rec aux = function 
     | Open (x, t) -> 
       fprintf fmt "@[<hov 2>open @[%a@]@;in@ @[%a@]@]" 
	 pp_ident x pp_term t
     | Define (defs, t) when is_empty defs ->
       pp_term fmt t
     | Define (defs, t) -> 
       fprintf fmt "@[defs @[%a@]@;in@ @[%a@]@]" 
	 pp_definitions defs pp_term t
    and pp_definitions fmt defs = 
      E.pp_environment ~show_fam:false pp_ident pp_obj pp_ty fmt defs
    in
    aux 

end
