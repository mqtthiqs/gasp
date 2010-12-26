let ($) f g x = g(f(x))

let rec list_map_prefix (f : 'b list -> 'a -> 'b)  (l : 'a list) : 'b list =
  let rec map p = function
    | [] -> p
    | x :: xs -> map (p @ [f p x]) xs
  in
  map [] l

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
