type t
val empty: t

val get_admission:
  firstname: string ->
  lastname:string ->
  year:string ->
  t -> Public_data.admission option

val add_admission:
  ((string * int * int * int) ->
   'state ->
   Public_data.admission ->
   Public_data.admission -> 'state * Public_data.admission) ->
  (string * int * int * int) -> 'state ->
  Public_data.admission ->
  t ->
  'state * t
