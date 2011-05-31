open Format

type level = int

type level_rel = level -> level -> bool

type 'a level_printing_fun = level_rel -> formatter -> 'a -> unit
type 'a printing_fun = formatter -> 'a -> unit

type 'a precedence = 'a -> level

let surround fmt f x = fprintf fmt "(@[%a@])" f x

let rec pr_paren term (prec:'a precedence) oldl (rel:level_rel) fmt t = 
  let newl = prec t in
  if rel newl oldl
  then term (pr_paren term prec newl) fmt t
  else surround fmt (term (pr_paren term prec newl)) t

let rec pr_list sep pr fmt = function
  | [] -> ()
  | [x] -> pr fmt x
  | x :: xs -> fprintf fmt "%a%a%a" pr x sep () (pr_list sep pr) xs

let pr_comma fmt () = fprintf fmt ","
let pr_dot fmt () = fprintf fmt "."
let pr_spc fmt () = fprintf fmt " "
