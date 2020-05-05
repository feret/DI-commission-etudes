val make:
  Remanent_state.t ->
  (Public_data.keywords * 'a) list ->
  Remanent_state.t *
  (string * int * int * int ->
   Remanent_state.t -> string -> Remanent_state.t * bool) *
  (string * int * int * int ->
   Remanent_state.t -> string -> Remanent_state.t * Public_data.keywords option) * 
  (string * int * int * int ->
   Remanent_state.t -> string -> Remanent_state.t * 'a option)
