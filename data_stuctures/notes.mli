type t = Public_data.note
val int_of_string: (string* int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * int option
val float_of_string: (string* int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * float option
val to_string: (string* int * int * int) -> ?force_dec_sep_to_dot:bool -> Remanent_state.t -> t -> Remanent_state.t * string
val of_string: (string* int * int * int) -> Remanent_state.t -> string -> Public_data.valide option ->  Remanent_state.t * t option
val better: t -> t -> bool
val comparable: t -> t -> bool
val valide: t -> bool option
val valide_forced: t -> bool  -> bool option 
val temporary: t -> bool option
val a_compter: t -> bool option
val string_of_ects: float option -> string
val compensable: t -> bool
val en_cours: t -> bool
val float_to_string_easy: float -> string
val valide_sans_note: string
