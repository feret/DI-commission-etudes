type method_handler = Exception_without_parameter.method_handler

val empty_error_handler : method_handler
val is_empty_error_handler : method_handler -> bool

val warn_with_exn :
  Loggers.t ->
  string ->
  safe_mode:bool ->
  method_handler ->
  string * int * 'a * 'b ->
  ?message:string -> exn -> (unit -> 'c) -> method_handler * 'c

val warn :
  Loggers.t ->
  string ->
  safe_mode:bool ->
  method_handler ->
  string * int * 'a * 'b ->
  ?message:string -> exn -> 'c -> method_handler * 'c


val check_point :
  (Loggers.t -> method_handler -> 'a -> ?message:string ->
   exn -> unit -> method_handler * unit) ->
  Loggers.t -> method_handler -> method_handler -> 'a -> ?message:string  -> exn -> method_handler


val print : Loggers.t -> string -> method_handler -> unit
