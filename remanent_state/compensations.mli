type t
val empty: t
val get_compensation:
  firstname:string -> lastname:string -> year:string -> codecours:string -> t -> Public_data.compensation option
val add_compensation:
  ((string * int * int * int) ->
   'state ->
   Public_data.compensation ->
   Public_data.compensation -> 'state * Public_data.compensation) ->
  (string * int * int * int) -> 'state ->
  Public_data.compensation -> t ->
  'state * t
