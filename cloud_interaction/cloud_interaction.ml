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

let get_dated_repository state =
  match
    Remanent_state.get_output_alias state
  with
  | state, Some x -> state, x
  | state, None ->
    begin
      let state, date = Remanent_state.get_launching_date state in
      let state,local_rep =
        Remanent_state.get_local_repository state
      in
      let state,gps_rep =
        Remanent_state.get_repository_to_dump_gps_files state
      in
      let rep =
        match local_rep,gps_rep with
        | "","" -> ""
        | "",x1 | x1,"" -> x1
        | x1,x2 -> Printf.sprintf "%s/%s" x1 x2
      in
      let state, current_dir = Safe_sys.getcwd __POS__ state in
      let state = Safe_sys.chdir __POS__ state rep in
      let state, f_exists =
        Safe_sys.file_exists __POS__ state date
      in
      let state =
        if f_exists then
          Safe_sys.command __POS__ state
            (Printf.sprintf "rm -rf %s" date)
      else
        state
      in
      let state = Safe_sys.command __POS__ state (Printf.sprintf "mkdir %s" date) in
      let state, courant =
        Remanent_state.get_output_alias_repository
          state
      in
      let state, f_exists =
        Safe_sys.file_exists __POS__ state courant
      in
      let state, is_target =
        match Remanent_state.get_target state
        with
        | state, None -> state, false
        | state, Some _ -> state, true
      in
      let state =
        if f_exists && not is_target 
        then
          let command =
            Printf.sprintf "rm -rf %s" courant
          in
          Safe_sys.command __POS__ state command
        else
          state
      in
      let full_output_rep, full_courant =
        match rep with
        | "" -> date,"courant"
        | _ -> Printf.sprintf "%s/%s" rep date,
               Printf.sprintf "%s/courant" rep
      in
      let state =
        Remanent_state.set_output_alias
          state
          (full_output_rep,full_courant)
      in
      let state = Safe_sys.chdir __POS__ state current_dir in
      state, (full_output_rep, full_courant)
    end

let make_output_dir ?alias t =
  match alias with
  | None ->
    begin
      match
        Remanent_state.get_output_alias t
      with
      | t, None ->
        get_dated_repository t
      | t, Some x -> t, x
    end
  | Some x -> t, x

let create_dynamic_link ?alias t =
  let state, (output_rep, alias_rep) =
    make_output_dir ?alias t
  in
  Safe_sys.command __POS__ state
        (Printf.sprintf "ln -sf %s %s" output_rep alias_rep)

let create_hard_copy ?alias t =
  let state, (output_rep, alias_rep) =
    make_output_dir ?alias t
  in
  Safe_sys.command __POS__ state
    (Printf.sprintf "cp -rf %s %s" output_rep alias_rep)

let make_current_repository ?alias t =
  let t, b =
    Remanent_state.get_cloud_support_dynamic_link t
  in
  if b then
    create_dynamic_link ?alias t
  else
    create_hard_copy ?alias t
