val get_list_from_a_file:
  'a Keywords_handler.preprocessed  ->
  'a ->
  Remanent_state.t -> string * string -> 'a list -> Remanent_state.t * 'a list

val get_list:
  repository:string ->
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

val lift:
  (Remanent_state.t -> 'a -> Remanent_state.t * 'b option) ->
  (Remanent_state.t -> 'c -> 'b -> Remanent_state.t * 'c) ->
  (Remanent_state.t -> 'b -> Remanent_state.t * string option) ->
  (string * int * int * int) ->
  string ->
  (
   Remanent_state.t -> 'a -> 'c -> Remanent_state.t * 'c) *
  (Remanent_state.t -> 'a -> Remanent_state.t * string option)

val lift_safe:
  ('a -> 'b option) ->
  ('c -> 'b -> 'c) ->
  ('b  -> string) ->
  (string * int * int * int) ->
  string ->
  (
   Remanent_state.t -> 'a -> 'c -> Remanent_state.t * 'c )
  * (Remanent_state.t -> 'a -> Remanent_state.t * string option)

val lift_opt:
  (Remanent_state.t -> 'a -> Remanent_state.t * 'b option) ->
  (Remanent_state.t -> 'c -> 'b option -> Remanent_state.t * 'c) ->
  (Remanent_state.t -> 'b -> Remanent_state.t * string) ->
  (Remanent_state.t -> 'a -> 'c -> Remanent_state.t * 'c) *
  (Remanent_state.t -> 'a -> Remanent_state.t * string option)

val lift_opt_safe :
  ('a -> 'b option) ->
  ('c -> 'b option -> 'c) ->
  ('b -> string ) ->
  (Remanent_state.t -> 'a -> 'c -> Remanent_state.t * 'c) *
  (Remanent_state.t -> 'a -> Remanent_state.t * string option)

val lift_pred:
  (Remanent_state.t -> 'a -> Remanent_state.t * 'b option) ->
  Remanent_state.t -> 'a -> Remanent_state.t * bool

val lift_pred_safe:
  ('a -> 'b option) ->
  Remanent_state.t -> 'a -> Remanent_state.t * bool

val collect_gen :
  ?repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?p:('a -> bool) ->
  compute_repository:(Remanent_state.t ->
                      Remanent_state.t * string) ->
  fun_default:
    (Remanent_state.t -> string option -> 'a -> Remanent_state.t * 'a) ->
  keywords_of_interest:Public_data.keywords list ->
  asso_list:(Public_data.keywords *
             (Remanent_state.t ->
              string option -> 'a -> Remanent_state.t * 'a)) list ->
  keywords_list:Public_data.keywords list ->
  init_state:'a ->
  empty_elt:'b ->
  add_elt:(string * int * int * int ->
           'b -> Remanent_state.t -> Remanent_state.t) ->
  mandatory_fields:((Remanent_state.t ->
                     'a -> Remanent_state.t * bool) *
                    string )
      list ->
  all_fields:
    (
      (Remanent_state.t -> 'a -> 'b -> Remanent_state.t * 'b)
      *
      (Remanent_state.t -> 'a -> Remanent_state.t * string option)
    ) list ->
  ?event_opt:Sco_remanent_state.Profiling.step_kind ->
  Remanent_state.t -> Remanent_state.t
