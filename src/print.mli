open Format

type level = int

type level_rel = level -> level -> bool

type 'a level_printing_fun = level_rel -> formatter -> 'a -> unit
type 'a printing_fun = formatter -> 'a -> unit

type 'a precedence = 'a -> level

val pr : 
  ('a level_printing_fun -> formatter -> 'a -> unit) ->
  'a precedence -> level -> 'a level_printing_fun
