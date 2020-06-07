type t = Public_data.note
val int_of_string: (string* int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * int option
val float_of_string: (string* int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * float option
val to_string: (string* int * int * int) -> ?force_dec_sep_to_dot:bool -> Remanent_state.t -> t -> Remanent_state.t * string
val of_string: (string* int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * t option
val compare: t -> t -> int
val valide: t -> bool option
val a_compter: t -> bool option
val string_of_ects: float option -> string
