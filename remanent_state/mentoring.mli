type t
val empty: t
val get_mentoring:
  year:string ->
  firstname:string ->
  lastname:string -> t -> Public_data.tutorat option
val add_mentoring:
  ((string * int * int * int ) ->
   'state ->
   Public_data.tutorat ->
   Public_data.tutorat -> 'state * Public_data.tutorat) ->
  (string * int * int * int) -> 'state -> Public_data.tutorat -> t -> 'state * t
