module List : sig
  include module type of List

  val index : 'a -> 'a list -> int

  val fold_map : ('b -> 'a -> 'b * 'c) -> 'b -> 'a list -> 'b * 'c list

end = struct
  include List

  let index a =
    let rec aux i = function
      | [] -> raise Not_found
      | x :: xs -> if x = a then i else aux (i+1) xs
    in aux 0

  let cons x xs = x :: xs


  let fold_map f init xs =
  let acc, ys = List.fold_left
    (fun (acc, ys) x ->
      let acc, y = f acc x in
      (acc, y :: ys)) (init, []) xs
  in
  acc, List.rev ys
end
