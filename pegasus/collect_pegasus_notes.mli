val get_pegasus_notes:
    ?repository:string ->
    ?prefix:string ->
    ?file_name:string -> Remanent_state.t -> Remanent_state.t

val get_pegasus_validations:
    ?repository:string ->
    ?prefix:string ->
    ?file_name:string -> Remanent_state.t -> Remanent_state.t
