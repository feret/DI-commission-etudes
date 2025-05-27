val get_pegasus_courses:
    ?repository:string ->
    ?prefix:string ->
    ?file_name:string -> Remanent_state.t -> Remanent_state.t

val make_dictionary: 
    Remanent_state.t -> Remanent_state.t 