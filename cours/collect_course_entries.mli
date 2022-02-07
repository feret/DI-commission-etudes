val get_course_entries:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val unify_course_entry:
  string * int * int * int ->
  Remanent_state.t ->
  Public_data.course_entry ->
  Public_data.course_entry ->
  Remanent_state.t * Public_data.course_entry
