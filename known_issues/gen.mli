type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

val dump_elts:
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  ?event_opt:Sco_remanent_state.Profiling.step_kind ->
  get:(Remanent_state.t -> Remanent_state.t * 'a list) ->
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
