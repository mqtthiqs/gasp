module ExtPprint = struct

  let as_string document = 
    let b = Buffer.create 13 in 
    Pprint.Buffer.pretty 0.8 80 b document;
    Buffer.contents b

end

module ExtString = struct

  let to_list s = 
    let a = ref [] in
    String.iter (fun c -> a := c :: !a) s;
    List.rev !a

  let from_char c = 
    String.make 1 c

end
