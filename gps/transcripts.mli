type gps_file
val get_gps_file:
    input:string * string ->
  Remanent_state.t -> Remanent_state.t * gps_file option
val export_transcript:
  output: string * string ->
  Remanent_state.t ->
  gps_file
  -> Remanent_state.t * (string * string) option 
