type 'a shared =
  {
    do_at_end_of_file:
      Remanent_state.t -> 'a -> 'a list ->
      Remanent_state.t * 'a list;
    do_at_end_of_array_line:
      Public_data.keywords option list ->
      Remanent_state.t -> 'a ->  'a ->
      'a list ->
      Remanent_state.t * 'a * 'a list;
    do_at_end_of_array:
      Public_data.keywords option list ->
      Remanent_state.t ->
      'a ->
      'a list ->
      Remanent_state.t * 'a * 'a list;
    flush:
      Remanent_state.t -> 'a -> 'a list -> Remanent_state.t * 'a list;
  }

type 'a specification =
  {
    keywords: Public_data.keywords list;
    of_interest: Public_data.keywords list;
    asso:
      (Public_data.keywords *
       (Remanent_state.t ->
        string option ->
        'a -> Remanent_state.t * 'a)) list;
    default:
      (Remanent_state.t ->
       string option ->
       'a -> Remanent_state.t * 'a) ;
    shared_functions: 'a shared
  }

type 'a preprocessed =
  {
    is_keyword:
      string * int * int * int ->
      Remanent_state.t -> string -> Remanent_state.t * bool;
    action:
      string * int * int * int ->
      Remanent_state.t -> string ->
      Remanent_state.t *
      (Remanent_state.t ->
       string option ->
       'a -> Remanent_state.t * 'a)
        option;
    translate :
      string * int * int * int ->
      Remanent_state.t ->
      string -> Remanent_state.t * Public_data.keywords option;
    flush_required :
      string * int * int * int ->
      Remanent_state.t -> string -> Remanent_state.t * bool option;
    shared: 'a shared
  }

val make:
  Remanent_state.t ->
  'a specification ->
  Remanent_state.t * 'a preprocessed
