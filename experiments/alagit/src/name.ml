type t = {
  salt   : int;      (* FIXME: Should be Big_int. *)
  prefix : string;
}

(** Not thread-safe. *)
let internal_counter = ref 0 

let to_string s = 
  match s.salt with
    | 0 -> s.prefix
    | _ -> s.prefix ^ "(" ^ string_of_int s.salt ^ ")"

let fresh s = incr internal_counter; {
  prefix = s; 
  salt   = !internal_counter
}

let cache = Hashtbl.create 13

let from_string s = try 
  Hashtbl.find cache s
with Not_found -> 
  let n = { prefix = s; salt = 0 } in
  Hashtbl.add cache s n;
  n
  

exception InternalNameAlreadyInUse
let unique_from_string s = 
  if Hashtbl.mem cache s then raise InternalNameAlreadyInUse;
  from_string s

let compare n1 n2 = 
  match compare n1.salt n2.salt with
    | 0 -> compare n1.prefix n2.prefix
    | x -> x

let hash n = Hashtbl.hash n.salt
