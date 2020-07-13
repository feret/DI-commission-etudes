type t
val empty: t
val get_cursus_exception:
  firstname:string -> lastname:string ->
  code_gps:string -> year: string
  -> t -> Public_data.cursus_exception option

val add_cursus_exception:
  ((string * int * int * int) ->
   'state ->
   Public_data.cursus_exception ->
         Public_data.cursus_exception -> 'state * Public_data.cursus_exception) ->
  (string * int * int * int) ->
  'state -> Public_data.cursus_exception ->
  t -> 'state * t

val dump: t -> unit
