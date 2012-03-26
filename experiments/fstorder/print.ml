open Format

type level = int

type level_rel = level -> level -> bool

type 'a level_printing_fun = level_rel -> formatter -> 'a -> unit
type 'a printing_fun = formatter -> 'a -> unit

type 'a precedence = 'a -> level

let surround fmt f x = fprintf fmt "@[(%a)@]" f x

let rec paren term (prec:'a precedence) oldl (rel:level_rel) fmt t = 
  let newl = prec t in
  if rel newl oldl
  then term (paren term prec newl) fmt t
  else surround fmt (term (paren term prec newl)) t

let rec list sep pr fmt = function
  | [] -> ()
  | [x] -> pr fmt x
  | x :: xs -> fprintf fmt "@[%a%a@]@ %a" pr x sep () (list sep pr) xs

let rec list_rev sep pr fmt = function
  | [] -> ()
  | [x] -> pr fmt x
  | x :: xs -> fprintf fmt "@[%a%a@]@ %a" (list sep pr) xs sep () pr x

let comma fmt () = fprintf fmt ","
let semi fmt () = fprintf fmt ";"
let dot fmt () = fprintf fmt "."
let spc fmt () = fprintf fmt "@ "
let str fmt s = fprintf fmt "%s" s
let int fmt i = fprintf fmt "%d" i
let opt f fmt = function
  | None -> fprintf fmt "None"
  | Some x -> fprintf fmt "Some %a" f x
let opt_under f fmt = function
  | None -> fprintf fmt "_"
  | Some x -> fprintf fmt "%a" f x
