open Format

(* Typing errors *)

let type_error pos msg = 
  let as_string f = 
    let b = Buffer.create 13 in 
    let fmt = formatter_of_buffer b in
    f fmt;
    pp_print_flush fmt ();
    Buffer.contents b in
  Error.error "during type checking" pos (as_string msg)

let not_bound pos x =
  type_error pos
    (fun fmt -> fprintf fmt "@[Variable %s is not bound.@]@." x)

let over_application pos =
  type_error pos
    (fun fmt -> fprintf fmt "@[This function is over-applied.@]")
