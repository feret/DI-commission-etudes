type t

val empty: t
val get_inscription:
  level:string -> ?dpt:Public_data.main_dpt -> year:string -> firstname:string -> lastname:string -> t -> Public_data.inscription option
val add_inscription:
  ((string * int * int * int) ->
   'state ->
   Public_data.inscription ->
   Public_data.inscription -> 'state * Public_data.inscription) ->
  (string * int * int * int) ->
  'state ->
  Public_data.inscription -> t ->
  'state * t
