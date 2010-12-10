open Format
open LF

let lprod = 200
let llam = 200
let lapp = 10

module P = Position

let constant fmt c = fprintf fmt "@[%s@]" c
let variable fmt x = fprintf fmt "@[%s@]" x

let rec fam' fmt = function
  | FConst c -> constant fmt c
  | FProd (Anonymous,a,b) -> 
      fprintf fmt "@[%a@ ->@ %a@]" fam a fam b
  | FProd (Name x,a,b) -> 
      fprintf fmt "@[{%a@ :@ %a}@ %a@]" variable (P.value x) fam a fam b
  | FLam (Anonymous,a,b) -> 
      fprintf fmt "@[[_@ :@ %a]@ %a@]" fam a fam b
  | FLam (Name x,a,b) -> 
      fprintf fmt "@[[%a@ :@ %a]@ %a@]" variable (P.value x) fam a fam b
  | FApp (a,t) ->
      fprintf fmt "@[%a@ %a@]" fam a obj t

and fam fmt a = fam' fmt (P.value a)

and obj' fmt = function
  | OConst c -> constant fmt c
  | OVar x -> variable fmt x
  | OLam (Anonymous,a,t) -> 
      fprintf fmt "@[[_@ :@ %a]@ %a@]" fam a obj t
  | OLam (Name x,a,t) ->
      fprintf fmt "@[[%a@ :@ %a]@ %a@]" variable (P.value x) fam a obj t
  | OApp (t,u) -> fprintf fmt "@[%a@ %a@]" obj t obj u

and kind' fmt = function
  | KType -> fprintf fmt "@[type@]"
  | KProd (Anonymous,a,k) -> 
      fprintf fmt "@[%a@ ->@ %a@]" fam a kind k
  | KProd (Name x,a,k) -> 
      fprintf fmt "@[{%a@ :@ %a}@ %a@]" variable (P.value x) fam a kind k

and kind fmt k = kind' fmt (P.value k)

and obj fmt t = obj' fmt (P.value t)

let rec sign fmt = function
  | [] -> ()
  | (c, EKind k) :: tl -> fprintf fmt "@[%a@ :@ %a@]@\n%a" constant c kind k sign tl
  | (c, EFam a) :: tl -> fprintf fmt "@[%a@ :@ %a@]@\n%a" constant c fam a sign tl
