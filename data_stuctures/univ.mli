val get_univ:
  diplome_dpt:Public_data.main_dpt ->
  diplome_niveau:string ->
  diplome_year:string ->
  firstname:string ->
  lastname:string -> 
  string list -> Remanent_state.t ->
  Remanent_state.t * Public_data.universite option * Public_data.cursus option
