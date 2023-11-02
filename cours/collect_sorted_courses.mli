val get_sorted_courses:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val unify_sorted_courses:
  string * int * int * int ->
  Remanent_state.t ->
  Public_data.cours_a_trier ->
  Public_data.cours_a_trier ->
  Remanent_state.t * Public_data.cours_a_trier
