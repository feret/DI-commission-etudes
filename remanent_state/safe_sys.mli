val file_exists: (string * int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * bool
val is_directory: (string * int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * bool
val rec_mk_when_necessary:
  (string * int * int * int) ->
  Remanent_state.t -> string -> Remanent_state.t * string

val readdir:
  (string * int * int * int) ->
  Remanent_state.t -> string -> Remanent_state.t * string array

val getcwd:
  (string * int * int * int) -> Remanent_state.t -> Remanent_state.t * string

val chdir:
  (string * int * int * int) -> Remanent_state.t -> string -> Remanent_state.t
val command:
  (string * int * int * int) ->
  Remanent_state.t -> string -> Remanent_state.t

val rm:
  (string * int * int * int) ->
  Remanent_state.t -> string -> Remanent_state.t

val cp:
  (string * int * int * int) ->
  Remanent_state.t -> string -> string -> Remanent_state.t

val get_extension:
  string -> string option

val is_empty:
  (string * int * int * int) ->
  Remanent_state.t -> string -> Remanent_state.t * bool 
