val get_list_from_a_file:
  (string * int * int * int ->
   Remanent_state.t -> string -> Remanent_state.t * bool)
  ->
  (string * int * int * int ->
   Remanent_state.t ->
   string ->
   Remanent_state.t *
   (Remanent_state.t ->
    string option -> 'a * bool -> Remanent_state.t * ('a * bool))
     option)
  ->
  (string * int * int * int ->
   Remanent_state.t -> string -> Remanent_state.t * Public_data.keywords option)
  ->
  (Public_data.keywords option list ->
   Remanent_state.t ->
   'a -> 'a -> bool -> 'a list -> Remanent_state.t * 'a * bool * 'a list)
  ->
  (Public_data.keywords option list ->
   Remanent_state.t ->
   'a -> bool -> 'a list -> Remanent_state.t * 'a * bool * 'a list)
  ->
  (Remanent_state.t -> 'a -> bool -> 'a list -> Remanent_state.t * 'a list)
  ->
  'a ->
  Remanent_state.t -> string * string -> 'a list -> Remanent_state.t * 'a list
