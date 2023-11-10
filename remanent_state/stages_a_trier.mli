type t
val empty: t

val get_sorted_internships:
  ?firstname:string ->
  ?lastname:string ->
  ?year:string ->
  ?libelle:string ->
  t -> Public_data.stage_a_trier list

val get_sorted_internship:
  firstname:string ->
  lastname:string ->
  year:string ->
  libelle:string ->
  t -> Public_data.stage_a_trier option

val add_sorted_internship:
  ((string * int * int * int) ->
   'state ->
   Public_data.stage_a_trier ->
   Public_data.stage_a_trier -> 'state * Public_data.stage_a_trier) ->
  (string * int * int * int) -> 'state ->
  Public_data.stage_a_trier ->
  t ->
  'state * t
