open MetaAST

let rec on f ty = function
  | Object x -> f x
  | Define (x, Object o) -> 
    begin match List.rev (f o) with
      | [] -> assert false
      | (k, v) :: ks -> List.rev ((k, v) :: (x, v) :: ks)
    end
  | Reference x -> 
    [ (Name.fresh "_", (ty, Some (Position.unknown_pos (AST.Var x)))) ]
  | _ -> assert false (* FIXME *)

let rec on_list nil cons = function
  | Nil -> 
    nil 
  | Cons (x, Object xs) -> 
    cons x (Object (on_list nil cons xs))
  | _ -> assert false (* FIXME *)


