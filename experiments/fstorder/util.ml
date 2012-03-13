module List = struct

  include List

  let index p =
    let rec aux i = function
      | [] -> raise Not_found
      | x :: xs -> if p x then i else aux (i+1) xs
    in aux 0


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

let (@@) a b = a b
let (<@) f g x = f (g x)
let (@>) f g x = g (f x)

let id x = x

type ('a, 'b) union =
  | Inl of 'a
  | Inr of 'b

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
