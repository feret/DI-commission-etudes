type t
val empty: t
val get_dpt:
  acronym:string -> t -> Public_data.dpt option
val add_dpt:
  safe_mode:bool ->
  Loggers.t ->
  string ->
  string * int * int * int ->
  Exception_without_parameter.method_handler ->
  Public_data.dpt ->
  t -> Exception_without_parameter.method_handler * t
