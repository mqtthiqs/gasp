module List = struct

  include List

  let index p =
    let rec aux i = function
      | [] -> raise Not_found
      | x :: xs -> if p x then i else aux (i+1) xs
    in aux 0

  let rec memp p x = function
    | [] -> false
    | a::l -> p (x, a) || memp p x l

  let fold_map f init xs =
  let acc, ys = List.fold_left
    (fun (acc, ys) x ->
      let acc, y = f acc x in
      (acc, y :: ys)) (init, []) xs
  in
  acc, List.rev ys

  let count i n =
    let rec aux i = if i=n then [] else i :: aux (i+1) in
    aux i

  let rec map_i i f = function
    | [] -> []
    | x :: xs -> f i x :: map_i (succ i) f xs

  let transpose l =
    let rec skip i l = if i=0 then l else None :: skip (pred i) l in
    let rec inv i j = function
      | [] -> []
      | x :: xs -> skip (x-j) (Some i :: inv (succ i) (x+1) xs)
    in inv 0 0 l

  let make f n =
    let rec aux i = if i=n then [] else f i :: aux (succ i) in
    aux 0

  let drop f xs is =
    let rec aux k = function
      | [], [] -> [], 0
      | x :: xs, i :: is when i=k ->
        let xs, s = aux (succ k) (xs, is) in
        f s x :: xs, 0
      | x :: xs, is ->
        let xs, s = aux (succ k) (xs, is) in
        xs, succ s
      | _ -> failwith "drop"
    in fst (aux 0 (xs, is))

end

module String = struct
  include String

  let subscript_of_char = function
    | '0' -> "₀"
    | '1' -> "₁"
    | '2' -> "₂"
    | '3' -> "₃"
    | '4' -> "₄"
    | '5' -> "₅"
    | '6' -> "₆"
    | '7' -> "₇"
    | '8' -> "₈"
    | '9' -> "₉"
    | _ -> invalid_arg "subscript_of_char"

  let subscript_of_int n =
    let s = string_of_int n in
    let rec loop i =
      try
        let x = subscript_of_char (String.get s i) in
        x ^ loop (succ i)
      with Invalid_argument _ -> ""
    in loop 0
  ;;

  subscript_of_int 42;;

end

module Prod = struct

  let map f g = fun (x, y) -> f x, g y

end

module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)
  let default a = function
    | Some y -> y
    | _ -> a
end

module Debug = struct

  open Format

  let tags = ref ["all"]

  let active tag =
    List.mem tag !tags
    || List.mem "all" !tags

  let formatter = formatter_of_out_channel stdout

  let flush = pp_print_newline formatter

  let _ =
    pp_open_vbox formatter 0;
    at_exit flush

  let stack = ref []

  let pop tag = function
    | tag' :: stack -> if tag = tag' then stack
      else failwith ("you must close tag "^tag'^" before closing "^tag)
    | [] -> failwith ("cannot close "^tag^": logging stack empty")

  let print_log tag fmt =
    kfprintf (kfprintf (fun fmt -> pp_close_box fmt ())) fmt "* %s: @[" tag

  let log tag =
    if active tag
    then kfprintf (print_log tag) formatter "@,"
    else ikfprintf ignore formatter

  let log_open tag =
    if active tag
    then begin
      stack := tag :: !stack;
      kfprintf (print_log tag) formatter "@,@[<v 2>"
    end else ikfprintf ignore formatter

  let log_close tag =
    if active tag
    then begin
      stack := pop tag !stack;
      kfprintf (print_log tag) formatter "@]@,"
    end else ikfprintf ignore formatter

  let close tag = if active tag then
      stack := pop tag !stack;
      pp_close_box formatter ()

end

let (@@) a b = a b
let (<@) f g x = f (g x)
let (@>) f g x = g (f x)

let id x = x

type ('a, 'b) union =
  | Inl of 'a
  | Inr of 'b

exception Located of Camlp4.PreCast.Loc.t * exn
