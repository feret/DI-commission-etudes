type t
val empty: t
val get_dens_candidate:
  firstname:string ->
  lastname:string -> t -> Public_data.dens_candidate list

val add_dens_candidate:
((string * int * int * int) ->
 'state ->
 Public_data.dens_candidate ->
 Public_data.dens_candidate -> 'state * Public_data.dens_candidate) ->
(string * int * int * int) -> 'state ->
Public_data.dens_candidate ->
t ->
'state * t
