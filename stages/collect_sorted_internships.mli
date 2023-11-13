val get_sorted_internships:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val unify_sorted_internships:
  string * int * int * int ->
  Remanent_state.t ->
  Public_data.stage_a_trier ->
  Public_data.stage_a_trier ->
  Remanent_state.t * Public_data.stage_a_trier
