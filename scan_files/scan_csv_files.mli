val get_list_from_a_file:
  'a Keywords_handler.preprocessed  ->
  'a ->
  Remanent_state.t -> string * string -> 'a list -> Remanent_state.t * 'a list

val get_list:
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  keywords_list:Public_data.keywords list ->
  asso_list:(Public_data.keywords *
                      (Remanent_state.t ->
                       string option -> 'a -> Remanent_state.t * 'a))
                     list ->
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
   'a list -> Remanent_state.t * 'a list
