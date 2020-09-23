type dpt = Maths | PE
type access_type =
    GPS of dpt option | Backup | Preempt | Warn

type mode =
  {
    access_type: access_type;
    avec_accent: bool
  }

val get_student_file :
  Public_data.student_id ->
  ?modelist:mode list ->
  ?file_retriever:Public_data.file_retriever ->
  ?command_line_options:string ->
  ?machine:string ->
  ?port:string ->
  ?input_repository:string ->
  ?output_repository:string ->
  ?prefix:string ->
  ?timeout:int option ->
  ?checkoutperiod:int ->
  ?output_file_name:string ->
  ?log_file:string ->
  ?log_repository:string ->
  ?user_name:string ->
  ?password:string ->
  Remanent_state.t -> Remanent_state.t * (string * string) option

val get_students_list:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?promotion:string ->
  Remanent_state.t ->
  Remanent_state.t

val patch_student_file:
  ?firstname:string ->
  ?lastname:string ->
  input:(string * string) ->
  output:(string * string) ->
  Remanent_state.t -> Remanent_state.t *  (string * string) option
