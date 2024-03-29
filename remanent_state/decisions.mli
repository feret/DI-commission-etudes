type t
val empty: t

val get_decision:
  firstname: string ->
  lastname:string ->
  ?year:string ->
  ?program: string ->
  ?dpt:Public_data.main_dpt ->
  t -> Public_data.decision list

val add_decision:
  (string * int * int * int -> string -> exn -> 'state -> 'state)
  ->
  ((string * int * int * int) ->
   'state ->
   Public_data.decision ->
   Public_data.decision -> 'state * Public_data.decision) ->
  (string * int * int * int) -> 'state ->
  Public_data.decision ->
  t ->
  'state * t
