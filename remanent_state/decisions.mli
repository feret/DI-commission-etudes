type t
val empty: t

val get_decision:
  firstname: string ->
  lastname:string ->
  year:string ->
  program: string ->
  dpt:string ->
  t -> Public_data.decision option

val add_decision:
  ((string * int * int * int) ->
   'state ->
   Public_data.decision ->
   Public_data.decision -> 'state * Public_data.decision) ->
  (string * int * int * int) -> 'state ->
  Public_data.decision ->
  t ->
  'state * t
