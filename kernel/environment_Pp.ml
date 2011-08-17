open Format

module Make (Environment : Environment.Sig) = struct

  open Environment

  let pp_environment ?(show_fam=true) pp_ident pp_term pp_ty fmt e = 
    let rec aux fmt = function
      | [] -> 
	()

      | (x, ty, None) :: e -> 
	if show_fam then 
	  fprintf fmt "@[@[%a :@;@[%a@]@]%s@;@[%a@]@]" 
	    pp_ident x pp_ty ty (if e = [] then "" else ", ") aux e
	else 
	  fprintf fmt "@[@[%a@]%s@;@[%a@]@]" 
	    pp_ident x (if e = [] then "" else ", ") aux e

      | (x, ty, Some t) :: e -> 
	if show_fam then 
	  fprintf fmt "@[@[<hov 2>%a =@;@[(%a@;:@ %a)@]@]%s@;@[%a@]@]" 
	    pp_ident x pp_term t pp_ty ty (if e = [] then "" else ", ") aux e
	else 
	  fprintf fmt "@[@[<hov 2>%a =@;@[%a@]@]%s@;@[%a@]@]" 
	    pp_ident x pp_term t (if e = [] then "" else ", ") aux e
    in
    aux fmt (as_list e)

end
