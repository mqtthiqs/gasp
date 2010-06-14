module ListExt = struct

  let get2 : string -> 'a list -> 'a * 'a = 
    fun msg -> function
      | [ x; y ] -> (x, y)
      | _ -> Error.global_error "internal" msg
	  
  let get3 : string -> 'a list -> 'a * 'a * 'a = 
    fun msg -> function
      | [ x; y; z ] -> (x, y, z)
      | _ -> Error.global_error "internal" msg

  let rec cut n l = 
    let rec aux n accu l = 
      match n, l with
	| 0, _  -> List.rev accu
	| _, [] -> raise Not_found
	| n, x :: xs -> aux (n - 1) (x :: accu) xs
    in
    aux n [] l
	  
end

module FilenameExt = struct

  let get_extension s = 
    try 
      let idx = String.rindex s '.' in
      String.sub s (idx + 1) (String.length s - idx - 1)
    with Not_found -> "" 

end

module IOExt = struct

  let open_out_with_backup fname = 
    if Sys.file_exists fname then begin
      (* FIXME: Too many syscalls. *)
      let rec find_free_name prefix i = 
	let fname = prefix ^ "~" ^ string_of_int i in
	if not (Sys.file_exists fname) then 
	  fname
	else find_free_name prefix (i + 1)
      in
      let backup_fname = find_free_name ("." ^ fname) 1 in
      if Sys.command (Printf.sprintf "cp %s %s" fname backup_fname) != 0 then begin
	Error.global_error "during backup" 
	  (Printf.sprintf "Problem when copying `%s'." fname)
      end
    end;
    open_out fname

end
