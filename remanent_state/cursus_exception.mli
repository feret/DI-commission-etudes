type t
val empty: t
val get_cursus_exception:
  firstname:string -> lastname:string ->
  code_gps:string -> year: string
  -> t -> Public_data.cursus_exception option

val add_cursus_exception:
  safe_mode:bool ->
  Loggers.t ->
  string ->
  string * int * int * int ->
  Exception_without_parameter.method_handler ->
  Public_data.cursus_exception ->
  t -> Exception_without_parameter.method_handler * t
val dump: t -> unit 
