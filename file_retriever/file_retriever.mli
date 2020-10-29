val launch :
  ?user_name:string ->
  ?password:string ->
  ?options:string ->
  ?log_file:string ->
  ?log_repository:string ->
  ?timeout:int ->
  Public_data.file_retriever ->
  url:string ->
  output_repository:string ->
  output_file_name:string ->
  Remanent_state.t -> Remanent_state.t * int

val check :
  ?log_file:string ->
  ?log_repository:string ->
  output_repository:string ->
  output_file_name:string ->
  period:int ->
  ?timeout:int ->
  Public_data.file_retriever ->
  Remanent_state.t ->
  Remanent_state.t
