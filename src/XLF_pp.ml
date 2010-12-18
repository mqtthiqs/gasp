open Format
open Pp
open XLF
open Name

type entity =
  | Kind of kind
  | Fam of fam
  | Obj of obj
  | Args of args
  | Entry of entry
  | Sign of sign

let kind_prec = function
  | KType -> 0
  | KProd _ -> 30
let fam_prec = function
  | FConst _ -> 30
  | FProd _ -> 30
let obj_prec = function
  | OLam _ -> 20
  | OVar _ -> 30
  | OConst _ -> 30
  | OApp _ -> 30
let args_prec = function
  | [] -> 0
  | _::_ -> 10
let entry_prec = function
  | FDecl _ -> 50
  | ODecl _ -> 50
let sign_prec = function
  | [] -> 0
  | _::_ -> 50
let ent_prec = function
  | Kind k -> kind_prec k
  | Fam a -> fam_prec a
  | Obj o -> obj_prec o
  | Args l -> args_prec l
  | Entry e -> entry_prec e
  | Sign s -> sign_prec s

let ident fmt x = fprintf fmt "@[%s@]" x

let pp pp fmt = function
  | Kind k -> begin match k with
      | KType -> fprintf fmt "@[type@]"
      | KProd (Named x,a,k) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
	  ident x (pp (<)) (Fam a) (pp (<=)) (Kind k)
      | KProd (Anonymous,a,k) -> fprintf fmt "@[%a@ ->@ %a@]" 
	  (pp (<)) (Fam a) (pp (<=)) (Kind k)
      end
  | Fam a -> begin match a with
      | FProd (Named x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
	  ident x (pp (<)) (Fam a) (pp (<=)) (Fam b)
      | FProd (Anonymous,a,b) -> fprintf fmt "@[%a@ ->@ %a@]" 
	  (pp (<)) (Fam a) (pp (<=)) (Fam b)
      | FConst (c,l,k) -> fprintf fmt "@[%a@ [%a]@ :@ %a@]"
	  ident c (pp (<)) (Args l) (pp (<)) (Kind k)
      end
  | Obj o -> begin match o with
      | OLam(Named x,a,t) -> fprintf fmt "@[[%a@ :@ %a]@ %a@]" 
	  ident x (pp (<=)) (Fam a) (pp (<=)) (Obj t)
      | OLam(Anonymous,a,t) -> fprintf fmt "@[[_@ :@ %a]@ %a@]" 
	  (pp (<=)) (Fam a) (pp (<=)) (Obj t)
      | OVar(x,l,a) | OConst(x,l,a) -> fprintf fmt "@[%a@ [%a]@ :@ %a@]"
	  ident x (pp (<)) (Args l) (pp (<)) (Fam a)
      | OApp(t,l,a) -> fprintf fmt "@[%a@ [%a]@ :@ %a@]"
	  (pp (<)) (Obj t) (pp (<)) (Args l) (pp (<)) (Fam a)
      end
  | Args l -> begin match l with
      |	[] -> ()
      | [t] -> fprintf fmt "@[%a@]" (pp (<)) (Obj t)
      | t :: l -> fprintf fmt "@[%a@ ;@ %a@]"
	  (pp (<)) (Obj t) (pp (<=)) (Args l)
    end
  | Entry e -> begin match e with
      | ODecl a -> (pp (<=)) fmt (Fam a)
      | FDecl k -> (pp (<=)) fmt (Kind k)
      end
  | Sign s -> begin match s with
      | [] -> ()
      | (c, e) :: s -> fprintf fmt "@[%a@ :@ %a@].@.%a"
	  ident c (pp (<=)) (Entry e) (pp (<=)) (Sign s)
    end

let sign fmt s = pr pp ent_prec 100 (<=) fmt (Sign s)
