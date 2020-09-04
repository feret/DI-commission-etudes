type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val dump_per_dpt_student_year: dump
val dump_per_dpt_class_year: dump
val dump_per_student: dump
