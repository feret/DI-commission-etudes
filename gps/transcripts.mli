type gps_file
val get_gps_file:
    input:string * string ->
  Remanent_state.t -> Remanent_state.t * gps_file option

val export_transcript:
  output: string * string ->
  ?language:Public_data.language ->
  ?number_of_diploma_per_page:int ->
  ?bilinguage:bool ->
  ?include_picture:bool ->
  ?repartition:Public_data.repartition ->
  ?signature:(string list)->
  ?report:bool ->
  ?filter: Public_data.remove_non_valided_classes ->
  ?keep_success:bool ->
  ?keep_faillure:bool ->
  Remanent_state.t ->
  gps_file
  -> Remanent_state.t * (string * string) option


  val export_transcript_export_scolarite :
    output: string * string ->
    cost_member: Public_data.cost_member ->
    ?language:Public_data.language ->
    ?bilinguage:bool ->
    ?signature:(string list)->
    Remanent_state.t ->
    gps_file
    -> Remanent_state.t * (string * string) option
