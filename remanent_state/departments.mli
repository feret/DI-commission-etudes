type t
val empty: t
val get_dpt:
  acronym:string -> t -> Public_data.dpt option
val add_dpt:
  ((string * int * int * int) ->
   'state -> Public_data.dpt -> Public_data.dpt -> 'state * Public_data.dpt) ->
  (string * int * int * int) ->
  'state ->
  Public_data.dpt ->
  t ->
  'state * t
    
