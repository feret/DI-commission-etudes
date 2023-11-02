type t
val empty: t

val get_sorted_courses:
  ?firstname:string ->
  ?lastname:string ->
  ?year:string ->
  ?libelle:string ->
  ?codegps:string ->
  t -> Public_data.cours_a_trier list

val get_sorted_course:
  firstname:string ->
  lastname:string ->
  year:string ->
  libelle:string ->
  codegps:string ->
  t -> Public_data.cours_a_trier option

val add_sorted_course:
  ((string * int * int * int) ->
   'state ->
   Public_data.cours_a_trier ->
   Public_data.cours_a_trier -> 'state * Public_data.cours_a_trier) ->
  (string * int * int * int) -> 'state ->
  Public_data.cours_a_trier ->
  t ->
  'state * t
