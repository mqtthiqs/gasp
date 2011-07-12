open Format

module Make (Environment : Environment.Sig) = struct

  open Environment

  let pp_environment pp_ident pp_term pp_ty fmt e = 
    let rec aux fmt = function
      | [] -> 
	()

      | (x, ty, None) :: e -> 
	fprintf fmt "@[@[%a :@,@[%a@]@]@,@[%a@]@]" 
	  pp_ident x pp_ty ty aux e

      | (x, ty, Some t) :: e -> 
	fprintf fmt "@[@[%a =@,@[%a : %a@]@]@,@[%a@]@]" 
	  pp_ident x pp_term t pp_ty ty aux e
    in
    aux fmt (as_list e)

end
