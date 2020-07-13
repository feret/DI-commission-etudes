type t
val empty: t
val get_scholarship:
  firstname:string ->
  lastname:string -> t -> Public_data.scholarship option

val add_scholarship:
((string * int * int * int) ->
 'state ->
 Public_data.scholarship ->
 Public_data.scholarship -> 'state * Public_data.scholarship) ->
(string * int * int * int) -> 'state ->
Public_data.scholarship ->
t ->
'state * t
