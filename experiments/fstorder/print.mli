open Format
open Util

type level = int

type level_rel = level -> level -> bool

type 'a level_printing_fun = level_rel -> formatter -> 'a -> unit
type 'a printing_fun = formatter -> 'a -> unit

type 'a precedence = 'a -> level

val paren : 
  ('a level_printing_fun -> formatter -> 'a -> unit) ->
  'a precedence -> level -> 'a level_printing_fun

val list : unit printing_fun -> 'a printing_fun -> 'a list printing_fun
val list_rev : unit printing_fun -> 'a printing_fun -> 'a list printing_fun

val comma : unit printing_fun
val semi : unit printing_fun
val dot : unit printing_fun
val spc : unit printing_fun 
val str : string printing_fun
val int : int printing_fun
val opt : 'a printing_fun -> 'a option printing_fun
val opt_under : 'a printing_fun -> 'a option printing_fun
val union : 'a printing_fun -> 'b printing_fun -> ('a, 'b) union printing_fun
