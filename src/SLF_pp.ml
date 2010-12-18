open Format
open Pp
open SLF

module P = Position

let term_prec a =  match P.value a with
  | Var _ -> 0
  | Type -> 0
  | App _ -> 10
  | Lam _ -> 20
  | Prod _ -> 30
  | Arr _ -> 30

let list_prec = function
  | [] -> 0
  | _::_ -> 50

let ident fmt x = fprintf fmt "@[%s@]" x

let pp_term pp fmt t = match P.value t with
  | Var x -> 
      ident fmt x
  | Arr (a,b) -> fprintf fmt "@[%a@ ->@ %a@]" 
      (pp (<)) a (pp (<=)) b
  | Prod (x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
      ident x (pp (<)) a (pp (<=)) b
  | Lam (x,a,b) -> fprintf fmt "@[[%a@ :@ %a]@ %a@]" 
      ident x (pp (<=)) a (pp (<=)) b
  | App (t,u) -> fprintf fmt "@[%a@ %a@]" 
      (pp (<=)) t (pp (<)) u
  | Type -> fprintf fmt "@[type@]"

let term fmt t = pr pp_term term_prec 100 (<=) fmt t

let pp_sign pp fmt = function
  | [] -> ()
  | (c, Decl t) :: tl -> fprintf fmt "@[%a@ :@ %a@].@.%a"
      ident c term t (pp (<=)) tl

let sign fmt s = pr pp_sign list_prec 100 (<=) fmt s
