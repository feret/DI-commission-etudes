val dump_missing_grades:
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  (Public_data.missing_grade -> Public_data.missing_grade -> int) ->
  string -> 
  Remanent_state.t ->
  Remanent_state.t

val per_dpt_class:
  Public_data.missing_grade ->
  Public_data.missing_grade ->
  int

val per_dpt_student:
  Public_data.missing_grade ->
  Public_data.missing_grade ->
  int
