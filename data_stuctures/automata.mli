val build:
  Remanent_state.t -> (string * 'a) list ->
  Remanent_state.t *
  (string * int * int * int -> Remanent_state.t -> string -> Remanent_state.t * bool) *
  (string * int * int * int ->
                      Remanent_state.t -> string -> Remanent_state.t * 'a option)
