let get_list_of_files ~repository ?prefix ?file_name state =
  let repository =
    match prefix with
    | None ->
      repository
    | Some prefix ->
      Printf.sprintf "%s/%s" repository prefix
  in
  match file_name with
  | Some file -> state, [repository,file]
  | None ->
    let rec explore state files_list rep_to_explore =
      match rep_to_explore with
      | [] ->
        state, files_list
      | h::t ->
        let _ =
          Format.printf "Scanning repository : %s @." h
        in
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
