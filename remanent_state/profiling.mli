type step_kind =
  | Dummy
  | Initialisation
  | Cloud_synchronization
  | Extract_gps_data_base
  | Extract_gps_file of string * string
  | Patch_gps_file of string option
  | Build_keywords_automaton
  | Export_transcript of string option
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
  | Dump_missing_grades 

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
