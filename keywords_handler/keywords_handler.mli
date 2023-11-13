type ('record_tmp,'record) any_field =
  { keyword: Public_data.keywords;
    set_tmp:
      Remanent_state.t ->
      string option -> 'record_tmp -> Remanent_state.t * 'record_tmp ;
    update:
      Remanent_state.t -> 'record_tmp -> 'record -> Remanent_state.t * 'record;
    is_unifyable: Remanent_state.t -> 'record -> 'record -> Remanent_state.t * bool ;
    unify: Remanent_state.t -> 'record -> 'record -> Remanent_state.t * 'record option;
    label_tmp: Remanent_state.t -> 'record_tmp -> Remanent_state.t * string option;
    label1:
      Remanent_state.t ->
      'record -> Remanent_state.t * string option;
    label2:
      Remanent_state.t ->
      'record -> 'record -> Remanent_state.t * string option
  }

type ('record_tmp) any_field_short =
  { key: Public_data.keywords;
    store:
      Remanent_state.t ->
      string option -> 'record_tmp -> Remanent_state.t * 'record_tmp}

val shorten:
  ('record_tmp,'record) any_field -> 'record_tmp any_field_short

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

type 'record_tmp specification =
  {
    keywords: Public_data.keywords list;
    of_interest: Public_data.keywords list;
    all_fields: 'record_tmp any_field_short list;
    default:
      (Remanent_state.t ->
       string option ->
       'record_tmp -> Remanent_state.t * 'record_tmp) ;
    shared_functions: 'record_tmp shared
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
  ?debug:bool -> Remanent_state.t ->
  'record_tmp specification ->
  Remanent_state.t * 'record_tmp preprocessed
