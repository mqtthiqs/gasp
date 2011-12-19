open Format

type level = int

type level_rel = level -> level -> bool

type 'a level_printing_fun = level_rel -> formatter -> 'a -> unit
type 'a printing_fun = formatter -> 'a -> unit

type 'a precedence = 'a -> level

val pr_paren : 
  ('a level_printing_fun -> formatter -> 'a -> unit) ->
  'a precedence -> level -> 'a level_printing_fun

val pr_list : unit printing_fun -> 'a printing_fun -> 'a list printing_fun

val pr_comma : unit printing_fun
val pr_dot : unit printing_fun
val pr_spc : unit printing_fun 
