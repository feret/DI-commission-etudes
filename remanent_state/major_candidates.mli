type t
val empty: t
val get_major_candidate:
  firstname:string ->
  lastname:string ->
  year:string ->
  ?dpt:Public_data.main_dpt -> t -> Public_data.mineure_majeure list

val add_major_candidate:
((string * int * int * int) ->
 'state ->
 Public_data.mineure_majeure ->
 Public_data.mineure_majeure -> 'state * Public_data.mineure_majeure) ->
(string * int * int * int) -> 'state ->
Public_data.mineure_majeure ->
t ->
'state * t
