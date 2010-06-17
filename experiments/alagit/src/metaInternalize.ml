let rec on f = function
  | MetaAST.Object x -> f x
  | _ -> assert false (* FIXME *)

let rec on_list nil cons = function
  | MetaAST.Nil -> 
    nil 
  | MetaAST.Cons (x, MetaAST.Object xs) -> 
    cons x (MetaAST.Object (on_list nil cons xs))
  | _ -> assert false (* FIXME *)


