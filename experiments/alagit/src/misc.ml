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

module FilenameExt = struct

  let get_extension s = 
    try 
      let idx = String.rindex s '.' in
      String.sub s (idx + 1) (String.length s - idx - 1)
    with Not_found -> "" 

end
