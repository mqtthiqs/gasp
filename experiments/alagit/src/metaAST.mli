type 'a t = 
  | Define    of AST.id * 'a t
  | Reference of AST.id
  | Apply     of AST.id * 'a t list
  | Object    of 'a

type 'a tlist =
  | Nil
  | Cons of 'a t * 'a tlist t
