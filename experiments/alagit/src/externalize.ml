let subnames env k = 
  try 
    Env.subnames env k 
  with Not_found -> 
    Error.global_error "during externalization"
      (Printf.sprintf "External name `%s' is unbound." (Name.to_string k))

let on_name env f k = 
  match subnames env k with
    | [] -> f k [] 
    | g :: xs -> f g xs

exception NotEnoughKeys

let from_names env f g = function
  | [] -> raise NotEnoughKeys
  | k :: ks -> 
    try 
      let y = match subnames env k with
	| [] -> f k [] 
	| k :: ks -> f k ks
      in
      g y ks
    with Not_found -> 
      Error.global_error "during externalization"
	(Printf.sprintf "External name `%s' is unbound." 
	   (Name.to_string k))

let match_key k l = 
  try 
    List.assoc k l
  with Not_found -> 
    Error.global_error "during externalization"
      (Printf.sprintf "Key `%s' is not in { %s }" 
	 (Name.to_string k)
	 (String.concat " " (List.map (fun (k, _) -> Name.to_string k) l))
      )


