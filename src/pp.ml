open Format

type level = int

type level_rel = level -> level -> bool

type 'a printing_fun = level_rel -> formatter -> 'a -> unit

type 'a precedence = 'a -> level

let surround fmt f x = fprintf fmt "(@[%a@])" f x

let rec pr term (prec:'a precedence) oldl (rel:level_rel) fmt t = 
  let newl = prec t in
  if rel newl oldl
  then term (pr term prec newl) fmt t
  else surround fmt (term (pr term prec newl)) t

let rec print prf prec (t:'a) =
  pr prf prec 100 (<) std_formatter t
