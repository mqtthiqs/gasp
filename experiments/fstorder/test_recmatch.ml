(* let eval x y = Some 42 *)

(* (\* usual match *\) *)
(* let _ = *)
(*   match None with *)
(*       Some x -> Some x *)
(*     | None -> None *)

(* (\* recursive match *\) *)
(* let _ = *)
(*   match* None in true with *)
(*     | Some x -> x *)

(* (\* usual let *\) *)
(* let _ = *)
(*   let (x,y) = (1,2) in x *)

(* (\* recursive let *\) *)
(* let _ = *)
(*   let* Some a in true = None in a *)
