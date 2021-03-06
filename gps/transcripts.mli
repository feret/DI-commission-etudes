type gps_file
val get_gps_file:
    input:string * string ->
  Remanent_state.t -> Remanent_state.t * gps_file option

val export_transcript:
  output: string * string ->
  ?signature:string ->
  ?report:bool ->
  ?filter: Public_data.remove_non_valided_classes ->
  Remanent_state.t ->
  gps_file
  -> Remanent_state.t * (string * string) option
