let synchronize_shared_repository
    ?cloud_client
    ?options
    ?local_repository
    ?distant_repository
    error_handling state =
  match
    Remanent_state.get_cloud_synchronization_mode
      state
  with
  | state, Public_data.Daemon -> state
  | state, Public_data.CommandLine ->
    begin
      let state, cloud_client =
        match cloud_client with
        | None -> Remanent_state.get_cloud_client state
        | Some x -> state, x
      in
      let state, options =
        match options with
        | None ->
          Remanent_state.get_cloud_client_options state
        | Some x -> state,x
      in
      let state, local_repository =
        match local_repository with
        | None ->
          Remanent_state.get_cloud_repository state
        | Some x -> state,x
      in
      let state, distant_repository =
        match distant_repository with
        | None ->
          Remanent_state.get_distant_repository state
        | Some x -> state, x
      in
      let cloud_client =
        match cloud_client with
        | Public_data.NextCloudCmd -> "nextcloudcmd"
      in
      let command =
        Format.sprintf
          "%s %s %s %s"
          cloud_client options local_repository
          distant_repository
      in
      let output = Sys.command command in
      match output with
      | 0 -> state
      | _ ->
        error_handling
          __POS__
          "Cloud synchronization failed"
          Exit
          state
    end

let synchronize_shared_repository
    ?cloud_client
    ?options
    ?local_repository
    ?distant_repository
    error_handling state =
  let state =
    Remanent_state.open_event_opt
      (Some Profiling.Cloud_synchronization)
      state
  in
  let state =
    synchronize_shared_repository
      ?cloud_client
      ?options
      ?local_repository
      ?distant_repository
      error_handling state
  in
  Remanent_state.close_event_opt
    (Some Profiling.Cloud_synchronization)
    state

let safe_synchronize_shared_repository =
  synchronize_shared_repository Remanent_state.stop
let synchronize_shared_repository =
  synchronize_shared_repository Remanent_state.warn
