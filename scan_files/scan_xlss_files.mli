val get_csv: string ->
          Remanent_state.t -> Remanent_state.t * string list list option

val get_list_from_a_file:
  strict:bool ->
  'a Keywords_handler.preprocessed  ->
  'a ->
  Remanent_state.t -> string * string -> 'a list -> Remanent_state.t * 'a list

val get_list:
  ?debug:bool ->
  strict:bool ->
  repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?automaton:'a Keywords_handler.preprocessed ->
  keywords_list:Public_data.keywords list ->
  all_fields:'a Keywords_handler.any_field_short list ->
  fun_default:(Remanent_state.t ->
               string option -> 'a -> Remanent_state.t * 'a) ->
  keywords_of_interest:Public_data.keywords list ->
  at_end_of_array:(Public_data.keywords option list ->
                   Remanent_state.t ->
                   'a -> 'a list -> Remanent_state.t * 'a * 'a list) ->
  at_end_of_file:(Remanent_state.t ->
                  'a -> 'a list -> Remanent_state.t * 'a list) ->
  at_end_of_array_line:(Public_data.keywords option list ->
                        Remanent_state.t ->
                        'a ->
                        'a ->
                        'a list -> Remanent_state.t * 'a * 'a list) ->
  flush:(Remanent_state.t ->
         'a -> 'a list -> Remanent_state.t * 'a list) ->
  init_state:'a ->
  Remanent_state.t ->
   'a list -> Remanent_state.t * 'a Keywords_handler.preprocessed option * 'a list

val collect_gen :
  ?debug:bool ->
  strict:bool ->
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?p:('a -> bool) ->
  compute_repository:(Remanent_state.t ->
                      Remanent_state.t * string) ->
  fun_default:
    (Remanent_state.t -> string option -> 'a -> Remanent_state.t * 'a) ->
  keywords_of_interest:Public_data.keywords list ->
  keywords_list:Public_data.keywords list ->
  init_state:'a ->
  empty_elt:'b ->
  add_elt:
    ((string * int * int * int ->
      Remanent_state.t -> 'b -> 'b -> Remanent_state.t * 'b) ->
     string * int * int * int ->
     'b -> Remanent_state.t -> Remanent_state.t)  ->
  mandatory_fields:'a Scan_gen_files.mandatory_field list ->
  all_fields:('a,'b) Keywords_handler.any_field list ->
  ?event_opt:Sco_remanent_state.Profiling.step_kind ->
  Remanent_state.t -> Remanent_state.t
