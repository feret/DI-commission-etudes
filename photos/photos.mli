val is_avalailable:
  firstname:string -> lastname:string -> promo:string ->
  Remanent_state.t -> Remanent_state.t * bool

val whereis:
  firstname:string -> lastname:string -> promo:string ->
  Remanent_state.t -> Remanent_state.t * string list

val fetch:
  ?user_name:string ->
  ?password:string ->
  ?file_retriever:Public_data.file_retriever ->
  ?command_line_options:string ->
  ?timeout:int option ->
  ?tries:int option -> 
  ?checkoutperiod:int ->
  ?url_to_access_annuaire: string -> 
  ?log_file:string ->
  ?log_repository:string ->
  ?tmp_repository:string ->
  ?tmp_file:string ->
  firstname:string ->
  lastname:string ->
  promo:string -> Remanent_state.t -> Remanent_state.t

val get:
  ?user_name:string ->
  ?password:string ->
  firstname:string ->
  lastname:string ->
  ?promo:string ->
  Remanent_state.t -> Remanent_state.t * string list
