
let rec list_fold_left_zip f acc p = function
  | [] -> acc
  | a::tl -> list_fold_left_zip f (f acc a p) (a::p) tl

