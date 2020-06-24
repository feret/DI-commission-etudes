val unsome: 'a option -> 'a -> 'a
val unsome_string: string option -> string

val remove_space_from_string:
  string -> string
val array_map_of_list : ('a -> 'b) -> 'a list -> 'b array

val asso_list_map2:
  'a list -> 'b list ->
  ('a -> 'c) -> ('b -> 'c) ->
  ('a -> 'd) -> ('b -> 'd) ->
  ('a -> 'b -> 'd) ->
  ('d list)

val date: unit -> string

val is_fully_capitalised: string -> bool

val map_opt:  ('a -> 'b) -> 'a option -> 'b option
val map_opt_state:  ('a -> 'b -> 'a * 'c) -> 'a -> 'b option -> 'a * ('c option)

val basename: string -> string

val space_only: string -> bool

val substring: string -> string -> bool 
