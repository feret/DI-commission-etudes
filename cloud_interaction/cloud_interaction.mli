val synchronize_shared_repository:
  Remanent_state.t -> Remanent_state.t

val safe_synchronize_shared_repository:
    Remanent_state.t -> Remanent_state.t

val create_dynamic_link:
  ?alias:string*string -> Remanent_state.t -> Remanent_state.t

val create_hard_copy:
  ?alias:string*string -> Remanent_state.t -> Remanent_state.t

val make_current_repository:
  ?alias:string*string -> Remanent_state.t -> Remanent_state.t

val get_dated_repository:
  Remanent_state.t -> Remanent_state.t * (string * string)
