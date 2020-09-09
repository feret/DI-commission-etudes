type dump =
  ?dpt:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?mentorname:string ->
  ?academicyear:string ->
  ?promo:string ->
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val dump_elts:
  ?dpt:string ->
  ?firstname:string ->
  ?lastname:string ->
  ?codegps:string ->
  ?mentorname:string ->
  ?academicyear:string ->
  ?promo:string ->
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?event_opt:Sco_remanent_state.Profiling.step_kind ->
  get:(Remanent_state.t -> Remanent_state.t * 'a list) ->
  filter:(?dpt:string ->
          ?firstname:string ->
          ?lastname:string ->
          ?codegps:string ->
          ?mentorname:string ->
          ?academicyear:string ->
          ?promo:string ->
          Remanent_state.t ->
          'a -> Remanent_state.t * bool) ->
  get_repository:(Remanent_state.t -> Remanent_state.t * string) ->
  default_file_name:string ->
  cmp:('a -> 'a -> int) list ->
  headers:(string * ('b -> string) * ('a -> 'b)) list ->
  columns:(string * ('a -> string)) list ->
  Remanent_state.t ->
  Remanent_state.t

val lift_cmp:
  ('a -> 'b) -> 'a -> 'a -> int
val op_cmp: ('a -> 'a -> int) -> 'a -> 'a -> int
