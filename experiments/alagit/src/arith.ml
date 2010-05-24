open AST

type expr =
  | O
  | S of expr
  | And of expr * expr
  | Or of expr * expr
  | Impl of expr * expr
  | Plus of expr * expr
  | Minus of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Le of expr * expr

let new_var() =
  let count = ref 0 in
  incr count; "*"^(string_of_int count)

let rec reify = function
  | O -> 
  | S a -> reify a
  | And (a,b) ->
  | Or (a,b) ->
  | Impl (a,b) ->
  | Plus (a,b) ->
  | Minus (a,b) ->
  | Eq (a,b) ->
  | Neq (a,b) ->
  | Le (a,b) ->
