
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
