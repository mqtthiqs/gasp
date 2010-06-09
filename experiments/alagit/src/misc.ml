module ListExt = struct

  let get2 : string -> 'a list -> 'a * 'a = 
    fun msg -> function
      | [ x; y ] -> (x, y)
      | _ -> Error.global_error "internal" msg
	  
  let get3 : string -> 'a list -> 'a * 'a * 'a = 
    fun msg -> function
      | [ x; y; z ] -> (x, y, z)
      | _ -> Error.global_error "internal" msg
	  
end
