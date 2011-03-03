include types of mli with

module Pp = struct
  open Format
  open Print
  open Name

  type entity =
    | Kind of kind
    | Fam of fam
    | Obj of obj
    | Head of ohead
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
    | OHead _ -> 20
  let head_prec = function
    | HConst _ -> 20
    | HVar _ -> 20
    | HMeta _ -> 20
    | HApp _ -> 30
  let args_prec = function
    | [] -> 0
    | _::_ -> 30
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
    | Head h -> head_prec h

  let ident fmt x = fprintf fmt "@[%s@]" x

  let pp pp fmt = function
    | Kind k -> begin match k with
	| KType -> fprintf fmt "@[type@]"
	| KProd (x,a,k) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
	    ident x (pp (<)) (Fam a) (pp (<=)) (Kind k)
      end
    | Fam a -> begin match a with
	| FProd (x,a,b) -> fprintf fmt "@[{%a@ :@ %a}@ %a@]" 
	    ident x (pp (<)) (Fam a) (pp (<=)) (Fam b)
	| FConst (c,l,k) -> fprintf fmt "@[%a@ [%a]@ :@ %a@]"
	    ident c (pp (<=)) (Args l) (pp (<)) (Kind k)
      end
    | Head h -> begin match h with
	| HApp t -> fprintf fmt "@[%a@]" (pp (<)) (Obj t)
	| HVar x | HConst x | HMeta x -> ident fmt x
      end
    | Obj o -> begin match o with
	| OLam(x,a,t) -> fprintf fmt "@[[%a@ :@ %a]@ %a@]" 
	    ident x (pp (<=)) (Fam a) (pp (<=)) (Obj t)
	| OHead(h,l,a) -> fprintf fmt "@[%a@ [%a]@ :@ %a@]"
	    (pp (<=)) (Head h) (pp (<=)) (Args l) (pp (<)) (Fam a)
      end
    | Args l -> begin match l with
	|	[] -> ()
	| [x,t] -> fprintf fmt "@[%a@ =@ %a@]" ident x (pp (<=)) (Obj t)
	| (x,t) :: l -> fprintf fmt "@[%a@ =@ %a@ ;@ %a@]"
	    ident x (pp (<=)) (Obj t) (pp (<=)) (Args l)
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
  let obj fmt s = pr pp ent_prec 100 (<=) fmt (Obj s)
  let fam fmt s = pr pp ent_prec 100 (<=) fmt (Fam s)
  let kind fmt s = pr pp ent_prec 100 (<=) fmt (Kind s)
  let args fmt s = pr pp ent_prec 100 (<=) fmt (Args s)
end
