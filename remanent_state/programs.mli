type t
val empty: t
val get_program:
  code_gps:string -> t -> Public_data.program option 
val add_program:
  safe_mode:bool ->
  Loggers.t ->
  string ->
  string * int * int * int ->
  Exception_without_parameter.method_handler ->
  Public_data.program ->
  t -> Exception_without_parameter.method_handler * t
