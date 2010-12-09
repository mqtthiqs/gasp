open Format
open LF

let constant fmt c = fprintf fmt "@[%s@]" c
let variable fmt s = fprintf fmt "@[%s@]" s

let rec fam' fmt = function
  | FConst c -> constant fmt c
  | FProd (Anonymous,a,b) -> 
      fprintf fmt "@[%a @->@ %a@]" fam a fam b
  | FProd (Name x,a,b) -> 
      fprintf fmt "@[{%a @:@ %a@}@ %a@]" variable x fam a fam b
  | FLam (Anonymous,a,b) -> 
      fprintf fmt "@[[_@:@ %a@]@ %a@]" fam a fam b
  | FLam (Name x,a,b) -> 
      fprintf fmt "@[[%a @:@ %a@]@ %a@]" variable x fam a fam b
  | FApp (a,t) ->
      fprintf fmt "@[%a %a@]" fam a obj t
