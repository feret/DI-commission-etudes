type t
val empty: t
val get_mentoring:
  year:string ->
  firstname:string ->
  lastname:string -> t -> Public_data.tutorat option
val add_mentoring:
  safe_mode:bool ->
  Loggers.t ->
  string ->
  string * int * int * int ->
  Exception_without_parameter.method_handler ->
  Public_data.tutorat ->
  t -> Exception_without_parameter.method_handler * t
