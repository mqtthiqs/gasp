open Format
open Print
module P = Position

include types of mli with 

module Pp = struct
    
  let term_prec a = match P.value a with
    | Type | Ident _
    | App _ -> 10
    | Lam _ -> 30
    | Prod _ -> 30
    | Arr _ -> 30
    | Box _ -> 30
	
  let list_prec = function
    | [] -> 0
    | _::_ -> 50
	
  let ident fmt x = fprintf fmt "@[%s@]" x

  let pp_term pp fmt t = 
    match P.value t with
    | Ident x -> ident fmt x
    | Arr (a,b) -> fprintf fmt "@[%a@ ->@ %a@]"
	(pp (<)) a (pp (<=)) b
    | Prod (x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
	ident x (pp (<=)) a (pp (<=)) b
    | Lam (x,b) -> fprintf fmt "@[[%a]]@ %a@]"
	ident x (pp (<=)) b
    | App (t,u) -> fprintf fmt "@[%a@ %a@]" 
	(pp (<=)) t (pp (<)) u
    | Box (t,None,s) -> fprintf fmt "@[{%a}%a@]" (pp (<=)) t (pp (<=)) s
    | Box (t,Some(x,n),s) -> fprintf fmt "@[{%a.%d@ =>@ %a}%a@]" ident x n (pp (<=)) t (pp (<=)) s
    | Type -> fprintf fmt "@[type@]"
	
  let term fmt t = pr_paren pp_term term_prec 100 (<=) fmt t
    
  let pp_sign pp fmt = function
    | [] -> ()
    | (c, t) :: tl -> fprintf fmt "@[%a@ :@ %a@].@.%a"
	ident c term t (pp (<=)) tl
	  
  let sign fmt s = pr_paren pp_sign list_prec 100 (<=) fmt s
    
end
