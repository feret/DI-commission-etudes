val get_student_file :
  Public_data.student_id ->
  ?file_retriever:Public_data.file_retriever ->
  ?command_line_options:string ->
  ?machine:string ->
  ?port:string ->
  ?input_repository:string ->
  ?output_repository:string ->
  ?prefix:string ->
  ?timeout:int option ->
  ?checkoutperiod:int ->
  ?file_name:string ->
  ?log_file:string ->
  ?log_repository:string ->
  ?user_name:string ->
  ?password:string ->
  Remanent_state.t -> Remanent_state.t * (string * string)

val get_students_list:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?promotion:string ->
  Remanent_state.t ->
  Remanent_state.t * Public_data.student_id list

val patch_student_file:
  input:(string * string) ->
  output:(string * string) ->
  Remanent_state.t -> Remanent_state.t *  (string * string) option
