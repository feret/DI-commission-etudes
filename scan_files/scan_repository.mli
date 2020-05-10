val get_list_of_files:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t * (string * string) list
