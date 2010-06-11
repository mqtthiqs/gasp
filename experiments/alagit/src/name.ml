type t = {
  salt   : int;      (* FIXME: Should be Big_int. *)
  prefix : string;
}

(** Not thread-safe. *)
let internal_counter = ref 0 

let fresh s = incr internal_counter; {
  prefix = s; 
  salt   = !internal_counter
}

let has_prefix s n = n.prefix = s

let cache = Hashtbl.create 13

let global_limit () = min_int + Hashtbl.length cache

let from_string s = try 
  Hashtbl.find cache s
with Not_found -> 
  let n = { prefix = s; salt = global_limit () } in
  Hashtbl.add cache s n;
  n

let to_string s = 
  if s.salt <= 0 then
    s.prefix
  else 
    s.prefix ^ "{" ^ string_of_int s.salt ^ "}"

let to_string_debug s = 
  s.prefix ^ "{" ^ string_of_int s.salt ^ "}"
  
exception InternalNameAlreadyInUse
let unique_from_string s = 
  if Hashtbl.mem cache s then raise InternalNameAlreadyInUse;
  from_string s

let compare n1 n2 = 
  match compare n1.salt n2.salt with
    | 0 -> compare n1.prefix n2.prefix
    | x -> x

let hash n = Hashtbl.hash n.salt
