open Format
open Print
open SLF

module P = Position

let term_prec a = match P.value a with
  | Type | Ident _ -> 0
  | App _ -> 10
  | Lam _ -> 30
  | Prod _ -> 30
  | Def _ -> 30
    
let list_prec = function
  | [] -> 0
  | _::_ -> 50
    
let ident fmt x = fprintf fmt "@[%s@]" x

module PpDef = Definitions_Pp.Make (Definitions)

let is_wildcard x = (x.[0] = '_')

let pp_term pp fmt t = 
  match P.value t with
    | Ident x -> 
      ident fmt x
    | Prod (x,a,b) when is_wildcard x ->
      fprintf fmt "@[%a@ ->@ %a@]"
	(pp (<)) a (pp (<=)) b
    | Prod (x,a,b) -> 
      fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
	ident x (pp (<=)) a (pp (<=)) b
    | Lam (x,ty,b) -> 
      fprintf fmt "@[[%s : %a]@] %a@]"
	x (pp (<=)) ty (pp (<=)) b
    | App (t,u) -> 
      fprintf fmt "@[%a@ %a@]" 
	(pp (<=)) t (pp (<)) u
    | Type -> 
      fprintf fmt "@[type@]"
    | Def f ->
      let pp_ty fmt = function None -> () | Some x -> pp (<=) fmt x in
      PpDef.pp_construct ident pp_ty (pp (<=)) (pp (<=)) fmt f
      
      
let term fmt t = pr_paren pp_term term_prec 100 (<=) fmt t
  
let args fmt l = pr_list pr_spc term fmt l


let pp_sign pp fmt = function
  | [] -> ()
  | (c, t) :: tl -> fprintf fmt "@[%a@ :@ %a@].@.%a"
    ident c term t (pp (<=)) tl

let sign fmt s = pr_paren pp_sign list_prec 100 (<=) fmt s
