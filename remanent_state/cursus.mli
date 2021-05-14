type t =
  Public_data.cursus
    Public_data.YearMap.t
    Public_data.DptOptMap.t
    Public_data.LevelMap.t

val empty: t
val get_cursus:
  level:string -> ?dpt:Public_data.main_dpt -> year:string ->
  t -> Public_data.cursus option
val add_cursus:
  ((string * int * int * int) ->
   'state ->
   Public_data.cursus ->
   Public_data.cursus -> 'state * Public_data.cursus) ->
  (string * int * int * int) ->
  'state ->
  Public_data.cursus -> t ->
  'state * t
