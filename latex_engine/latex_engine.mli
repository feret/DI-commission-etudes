val latex_to_pdf:
  ?rev:bool -> ?times:int -> input:(string * string) ->
  Remanent_state.t -> Remanent_state.t

val latex_opt_to_pdf:
  ?rev:bool -> ?times:int -> input:(string * string) option ->
    Remanent_state.t -> Remanent_state.t
