module List = struct

  include List

  let index a =
    let rec aux i = function
      | [] -> raise Not_found
      | x :: xs -> if x = a then i else aux (i+1) xs
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
end
