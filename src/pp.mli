open Format

type level = int

type level_rel = level -> level -> bool

type 'a printing_fun = level_rel -> formatter -> 'a -> unit

type 'a precedence = 'a -> level

val pr : 
  ('a printing_fun -> formatter -> 'a -> unit) ->
  'a precedence -> level -> 'a printing_fun
