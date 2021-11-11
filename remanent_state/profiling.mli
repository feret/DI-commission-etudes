type step_kind =
  | Dummy
  | Initialisation
  | Cloud_synchronization
  | Extract_gps_data_base
  | Extract_gps_file of string * string
  | Extract_gps_file_from_handmade_files of string * string
  | Extract_gps_file_from_backup_files of string * string
  | Extract_gps_file_from_database of string * string * string option
  | Collect_picture of string * string * string
  | Collect_picture_from_url of string * string * string * string
  | Collect_record_from_url of string * string * string * string
  | Patch_gps_file of string option
  | Build_keywords_automaton
  | Export_transcript of string option
  | Collect_additional_courses
  | Collect_admissions
  | Collect_scholarships
  | Collect_mentoring
  | Collect_departement
  | Collect_program
  | Collect_cursus_exceptions
  | Collect_decisions
  | Collect_dispenses
  | Collect_compensations
  | Collect_cursus
  | Collect_course_exceptions
  | Collect_modified_grade
  | Collect_course_name_translations 
  | Dump_missing_grades
  | Dump_missing_ects_attributions
  | Dump_missing_mentors
  | Dump_missing_internship_descriptions
  | Dump_ambiguous_internship_descriptions
  | Dump_mentor_list
  | Dump_national_diploma_list
  | Dump_dens_result
  | Dump_student_list

type log_info

val log_info_to_json: log_info -> Yojson.Basic.t
val log_info_of_json: Yojson.Basic.t -> log_info
val reset_log: log_info -> log_info
val dump_log:
  Loggers.t -> log_info -> unit

val is_dummy: step_kind -> bool

val open_event:
  Loggers.t ->
  string ->
  safe_mode:bool ->
  Exception_without_parameter.method_handler ->
  step_kind ->
  log_info -> Exception_without_parameter.method_handler * log_info

val close_event:
  Loggers.t ->
  string ->
  safe_mode:bool ->
  Exception_without_parameter.method_handler ->
  step_kind ->
  log_info -> Exception_without_parameter.method_handler * log_info

val open_event_opt:
  Loggers.t ->
  string ->
  safe_mode:bool ->
  Exception_without_parameter.method_handler ->
  step_kind option ->
  log_info -> Exception_without_parameter.method_handler * log_info


val close_event_opt:
  Loggers.t ->
  string ->
  safe_mode:bool ->
  Exception_without_parameter.method_handler ->
  step_kind option ->
  log_info -> Exception_without_parameter.method_handler * log_info

val set_time: log_info -> log_info
val ellapsed_global_time: log_info -> float
val ellapsed_time: log_info -> float
val init_log_info: unit -> log_info
val close_logger: Loggers.t -> unit
val flush_logger: Loggers.t -> unit
