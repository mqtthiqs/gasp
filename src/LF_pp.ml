open Format
open Pp
open LF

module P = Position

type entities =
  | Fam of fam
  | Obj of obj
  | Kind of kind
  | Sign of signature

let fam_prec a =  match P.value a with
  | FConst _ -> 0
  | FProd _ -> 30
  | FLam _ -> 20
  | FApp _ -> 10

let obj_prec t = match P.value t with
  | OConst _ -> 0
  | OVar _ -> 0
  | OLam _ -> 20
  | OApp _ -> 10

let kind_prec k = match P.value k with
  | KType -> 0
  | KProd _ -> 30

let list_prec = function
  | [] -> 0
  | _::_ -> 50

let entities_prec = function
  | Fam a -> fam_prec a
  | Obj t -> obj_prec t
  | Kind k -> kind_prec k
  | Sign s -> list_prec s

let constant fmt c = fprintf fmt "@[%s@]" c
let variable fmt x = fprintf fmt "@[%s@]" x

let pp pp fmt t =
  let fam rel fmt a = pp rel fmt (Fam a) in
  let obj rel fmt a = pp rel fmt (Obj a) in
  let kind rel fmt a = pp rel fmt (Kind a) in
  let signature rel fmt a = pp rel fmt (Sign a) in

  match t with
    | Fam a -> 
	begin
	  match P.value a with
	    | FConst c -> 
		constant fmt c
	    | FProd (Anonymous,a,b) -> fprintf fmt "@[%a@ ->@ %a@]" 
		(fam (<)) a (fam (<=)) b
	    | FProd (Name x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
		variable (P.value x) (fam (<)) a (fam (<=)) b
	    | FLam (Anonymous,a,b) -> fprintf fmt "@[[_@ :@ %a]@ %a@]" 
		(fam (<)) a (fam (<=)) b
	    | FLam (Name x,a,b) -> fprintf fmt "@[[%a@ :@ %a]@ %a@]" 
		variable (P.value x) (fam (<=)) a (fam (<=)) b
	    | FApp (a,t) -> fprintf fmt "@[%a@ %a@]" 
		(fam (<=)) a (obj (<)) t
	end

    | Obj t ->
	begin
	  match P.value t with
	  | OConst c -> constant fmt c
	  | OVar x -> variable fmt x
	  | OLam (Anonymous,a,t) -> fprintf fmt "@[[_@ :@ %a]@ %a@]" 
	      (fam (<)) a (obj (<=)) t
	  | OLam (Name x,a,t) -> fprintf fmt "@[[%a@ :@ %a]@ %a@]" 
	      variable (P.value x) (fam (<=)) a (obj (<)) t
	  | OApp (t,u) -> fprintf fmt "@[%a@ %a@]" 
	      (obj (<=)) t (obj (<)) u
	end
    | Kind k ->
	begin
	  match P.value k with
	    | KType -> fprintf fmt "@[type@]"
	    | KProd (Anonymous,a,k) -> fprintf fmt "@[%a@ ->@ %a@]" 
		(fam (<)) a (kind (<=)) k
	    | KProd (Name x,a,k) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
		variable (P.value x) (fam (<)) a (kind (<=)) k
	end

    | Sign s ->
	begin
	  match s with
	    | [] -> ()
	    | (c, EKind k) :: tl -> fprintf fmt "@[%a@ :@ %a@].@.%a"
		constant c (kind (<=)) k (signature (<=)) tl
	    | (c, EFam a) :: tl -> fprintf fmt "@[%a@ :@ %a@].@.%a"
		constant c (fam (<=)) a (signature (<=)) tl
	end

let fam fmt t = pr pp entities_prec 100 (<=) fmt (Fam t)
let obj fmt t = pr pp entities_prec 100 (<=) fmt (Obj t)
let kind fmt t = pr pp entities_prec 100 (<=) fmt (Kind t)
let signature fmt t = pr pp entities_prec 100 (<=) fmt (Sign t)
