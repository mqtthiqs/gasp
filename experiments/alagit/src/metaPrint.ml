open MetaAST

let on fmt f = function
  | Object x -> f fmt x
  | _ -> assert false (* FIXME *)
