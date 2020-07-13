type t
val empty: t
val get_program:
  code_gps:string -> t -> Public_data.program option
val add_program:
  ((string * int * int * int) ->
   'state ->
   Public_data.program ->
   Public_data.program -> 'state * Public_data.program) ->
  (string * int * int * int) ->
  'state ->
  Public_data.program -> t ->
  'state * t
