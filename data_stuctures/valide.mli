type t = Public_data.valide
val to_string: (string* int * int * int) ->  Remanent_state.t -> t -> Remanent_state.t * string
val of_string: ?context:string -> (string* int * int * int) -> Remanent_state.t -> string -> Remanent_state.t * t option
val valide: t -> bool option
