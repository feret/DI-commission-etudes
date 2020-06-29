val get_dpt:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val get_programs:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val get_cursus_exceptions:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t
