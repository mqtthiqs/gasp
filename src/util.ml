
let rec list_map_prefix (f : 'b list -> 'a -> 'b)  (l : 'a list) : 'b list =
  let rec map p = function
    | [] -> p
    | x :: xs -> map (p @ [f p x]) xs
  in
  map [] l
