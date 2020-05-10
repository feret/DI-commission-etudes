let get_list_of_files ?repository ?prefix ?file_name state =
  let state, repository =
    match repository, prefix with
    | None,None ->
      Remanent_state.get_students_list_repository state
    | Some rep, None ->
      let state,prefix =
        Remanent_state.get_students_list_prefix state in
      state, Printf.sprintf "%s/%s" rep prefix
    | None, Some prefix ->
      let state,rep =
        Remanent_state.get_local_repository state in
      state, Printf.sprintf "%s/%s" rep prefix
    | Some rep, Some prefix ->
      state, Printf.sprintf "%s/%s" rep prefix
  in
  match file_name with
  | Some file -> state, [repository,file]
  | None ->
    let rec explore state files_list rep_to_explore =
      match rep_to_explore with
      | [] ->
        state, files_list
      | h::t ->
        let state, to_explore = Safe_sys.readdir __POS__ state h in
        let state, files_list, rep_to_explore =
          Array.fold_left
            (fun (state,files_list,rep_list) file_to_explore
              ->
                let complete =
                  Printf.sprintf
                    "%s/%s" h file_to_explore
                in
                let state, bool =
                  Safe_sys.is_directory __POS__ state complete
                in
                if
                  bool
                then
                  (state, files_list,complete::rep_list)
                else
                  (state, (h,file_to_explore)::files_list,rep_list))
            (state, files_list,t)
            to_explore
        in
        explore state files_list rep_to_explore
    in
    explore state [] [repository]
