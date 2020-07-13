type t
val empty: t

val get_dispense:
  firstname: string ->
  lastname:string ->
  year:string ->
  program: string ->
  dpt:string ->
  t -> Public_data.dispense option

val add_dispense:
  ((string * int * int * int) ->
   'state ->
   Public_data.dispense ->
   Public_data.dispense -> 'state * Public_data.dispense) ->
  (string * int * int * int) -> 'state ->
  Public_data.dispense ->
  t ->
  'state * t
