module List = struct

  include List

  let rec last = function
    | [] -> invalid_arg "last"
    | [x] -> x
    | x :: xs -> last xs

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

  let rec fold_map f e = function
    |  []  -> (e,[])
    |  h::t ->
        let e',h' = f e h in
        let e'',t' = fold_map f e' t in
        e'',h'::t'
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

let split c s =
  let len = String.length s in
  let rec split n =
    try
      let pos = String.index_from s n c in
      let dir = String.sub s n (pos-n) in
      dir :: split (succ pos)
    with
      | Not_found -> [String.sub s n (len-n)]
  in
  if len = 0 then [] else split 0

end

module Int = struct

  open Char

  let alpha n =
    if n >= 0 && n <= 25 then chr (code 'a' + n)
    else if n >= 26 && n <= 36 then chr (code '0' + n - 26)
    else failwith ("alpha "^string_of_int n)

  let uident_of n =
    let rec loop base shift n =
      String.make 1 (alpha ((n mod base) + shift))
      ^ if n/base = 0 then "" else loop 36 0 ((n/base)-1)
    in if n>=0 then loop 13 0 n else loop 13 13 (-n)

  let _ =
    assert (uident_of 0 = "a");
    assert (uident_of 1 = "b");
    assert (uident_of (-1) = "o");
    assert (uident_of (13) = "aa");
    assert (uident_of (-13) = "na");
    assert (uident_of (-13*37) = "naa");
    assert (uident_of (13*37) = "aaa");

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

  let tags = ref (String.split ',' (try Unix.getenv "DEBUG" with Not_found -> ""))

  let active tag =
    List.mem "all" !tags
    || List.mem tag !tags

  let formatter = formatter_of_out_channel stdout
  let formatter = std_formatter

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

module Topcatch = struct

  open Format

  exception Unhandled

  let stk = ref ([] : (formatter -> exn -> unit) list)

  let register f = stk := f :: !stk

  let print fmt e =
    let rec aux = function
      | [] -> Format.pp_print_newline fmt (); raise e
      | f :: stk -> try f fmt e with Unhandled -> aux stk in
    aux !stk

  let catch fct arg =
    try
      fct arg
    with x ->
      pp_print_newline Debug.formatter ();
      flush stdout;
      eprintf "@[Uncaught exception:@ @[%a@]@]@." print x;
      raise Unhandled

  let _ =
    register begin fun fmt -> function
      | e  -> raise e
    end

end

let (@@) a b = a b
let (<@) f g x = f (g x)
let (@>) f g x = g (f x)

let id x = x

type ('a, 'b) union =
  | Inl of 'a
  | Inr of 'b

exception Located of Camlp4.PreCast.Loc.t * exn

let _ =
  Topcatch.register begin fun fmt -> function
    | Invalid_argument s -> Format.fprintf fmt "Invalid argument:@ %s" s
    | Located (l, e) -> Format.fprintf fmt "@[%a@]:@ %a" Camlp4.PreCast.Loc.print l Topcatch.print e
    | _ -> raise Topcatch.Unhandled
  end
