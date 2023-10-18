val split_courses: Public_data.dens -> Remanent_state.t -> Remanent_state.t * Public_data.dens
val split_stages: Public_data.dens -> Remanent_state.t -> Remanent_state.t * Public_data.dens
val collect_mineure: Public_data.dens -> Remanent_state.t -> Remanent_state.t * Public_data.dens
val get_dens_candidates: ?repository:string ->
                         ?prefix:string ->
                         ?file_name:string -> Remanent_state.t -> Remanent_state.t

val get_mineures_candidates: ?repository:string ->
                              ?prefix:string ->
                                                  ?file_name:string -> Remanent_state.t -> Remanent_state.t

val get_majeures_candidates: ?repository:string ->
                          ?prefix:string ->
                                                                                                    ?file_name:string -> Remanent_state.t -> Remanent_state.t

val dump_dens: Public_data.dens -> Remanent_state.t -> Remanent_state.t
val repeatable: Remanent_state.t -> string -> bool -> Remanent_state.t * bool
val suggest_mineure: Public_data.dens -> Remanent_state.t -> Remanent_state.t
val suggest_majeure: Public_data.dens -> Remanent_state.t -> Remanent_state.t
val suggest_candidate: Public_data.dens -> Remanent_state.t -> Remanent_state.t
