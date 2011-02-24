open Format
open Print
module P = Position

include types of mli with 

module Idmap = Map.Make(struct type t = ident let compare=Pervasives.compare end) and 

module Pp = struct
    
  let term_prec a =  match P.value a with
    | Type | Var _ | Meta _ -> 0
    | App _ -> 10
    | Lam _ -> 30
    | Prod _ -> 30
    | Arr _ -> 30
	
  let list_prec = function
    | [] -> 0
    | _::_ -> 50
	
  let ident fmt x = fprintf fmt "@[%s@]" x
    
  let pp_term pp fmt t = match P.value t with
    | Var x | Meta x -> 
	ident fmt x
    | Arr (a,b) -> fprintf fmt "@[%a@ ->@ %a@]" 
	(pp (<)) a (pp (<=)) b
    | Prod (x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
	ident x (pp (<=)) a (pp (<=)) b
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
    
end
  
  

let rec equals_term subst t u = 
  match P.value t, P.value u with
    | Type, Type -> true
    | Prod(x,a,b), Prod(x',a',b') ->
	equals_term subst a a' && equals_term (Idmap.add x x' subst)b b'
    | Arr(a,b), Arr(a',b')
    | Prod(_,a,b), Arr(a',b') 
    | Arr(a,b), Prod (_,a',b') -> 
	equals_term subst a a' && equals_term subst b b'
    | Lam(x,a,b), Lam(x',a',b') ->
	x=x' && equals_term subst a a' && equals_term (Idmap.add x x' subst) b b'
    | App(t,u), App(t',u') -> equals_term subst t t' && equals_term subst u u'
    | Var x, Var x' -> 
	(try x = Idmap.find x' subst
	with Not_found -> false)
    | _, Lam(x,_,{P.value=App(u, {P.value=Var x'})}) (* Eta *)
    | Lam(x,_,{P.value=App(u, {P.value=Var x'})}), _ when x=x' ->
	equals_term subst t u
    | _ -> false

let rec equals_sign subst s s' = 
  match s, s' with
    | [], [] -> true
    | (x, Decl t) :: s, (y, Decl u) :: s' ->
	equals_term subst t u && equals_sign (Idmap.add x y subst) s s'
    | _ -> false


