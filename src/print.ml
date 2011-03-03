include types of mli

open Format

let surround fmt f x = fprintf fmt "(@[%a@])" f x

let rec pr term (prec:'a precedence) oldl (rel:level_rel) fmt t = 
  let newl = prec t in
  if rel newl oldl
  then term (pr term prec newl) fmt t
  else surround fmt (term (pr term prec newl)) t
