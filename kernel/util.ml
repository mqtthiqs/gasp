let (//) f g x = g(f(x))

let rec list_map_prefix (f : 'b list -> 'a -> 'b) (p : 'b list) : 'a list -> 'b list = function
| [] -> []
| x :: xs -> 
    let x' = f p x in
    x' :: list_map_prefix f (x'::p) xs

(* fold_map : ('b -> 'a -> 'b * 'c) -> 'a list -> 'b -> 'b * 'c list *)
let list_fold_map f init xs =
  let accu, ys = List.fold_left
    (fun (accu, ys) x ->
      let accu, y = f accu x in
      (accu, y :: ys)) (init, []) xs
  in
  accu, List.rev ys


let buffer_of_file filename =
  let b = Buffer.create 16 in
  let ic = open_in filename in
  let rec read () = 
    try Buffer.add_channel b ic 1; read()
    with End_of_file -> ()
  in read (); b

let read_in_buffer b =
  let off = ref 0 in
  fun s n ->
    let rest = min (Buffer.length b - !off) n in
    if rest <= 0 then 0 else
      (Buffer.blit b !off s 0 rest; off := !off + rest; rest)

let if_debug f = if !Settings.debug then f ()

let curry f x y = f (x,y)

module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)
end

module Stringset = Set.Make(struct type t = string let compare = Pervasives.compare end)
