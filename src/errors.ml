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

let over_application t =
  type_error (Position.position t)
    (fun fmt -> fprintf fmt "@[The function %a is over-applied.@]" SLF_pp.term t)

let bad_application t =
  type_error (Position.position t)
    (fun fmt -> fprintf fmt "@[The application %a is invalid.@]" SLF_pp.term t)

let not_a s t =
  type_error (Position.position t)
    (fun fmt -> fprintf fmt "@[The term %a is not a %s.@]" SLF_pp.term t s)

let not_a_kind = not_a "kind"
let not_a_fam = not_a "family"
let not_an_obj = not_a "object"
let not_a_fam_or_obj = not_a "family or object"
let not_a_kind_or_fam = not_a "kind or family"
