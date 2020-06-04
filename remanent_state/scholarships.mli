type t
val empty: t
val get_scholarship:
  firstname:string ->
  lastname:string -> t -> Public_data.scholarship option
val add_scholarship:
  safe_mode:bool ->
  Loggers.t ->
  string ->
  string * int * 'a * 'b ->
  Exception_without_parameter.method_handler ->
  Public_data.scholarship ->
  t -> Exception_without_parameter.method_handler * t
