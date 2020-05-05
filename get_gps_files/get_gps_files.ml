let get_student_file
    student_id
    ?file_retriever ?command_line_options ?machine ?port ?input_repository ?output_repository ?prefix ?timeout ?checkoutperiod
    ?file_name ?log_file ?log_repository
    ?user_name ?password
    state
  =
  let firstname = student_id.Public_data.firstname in
  let lastname = student_id.Public_data.lastname in
  let promotion = student_id.Public_data.promotion in
  let state, log_file =
    match log_file with
    | None ->
      let state, x =
        Remanent_state.get_file_retriever_log_file state
      in
      state, Some x
    | Some x -> state,Some x
  in
  let state, log_repository =
    match log_repository with
    | None ->
      let state, x = Remanent_state.get_file_retriever_log_repository state in
      state, Some x
    | Some x -> state, Some x
  in
  let state, file_retriever =
    match file_retriever with
    | None ->
      Remanent_state.get_file_retriever state
    | Some wget -> state, wget
  in
  let state, options =
    match command_line_options with
    | None ->
      Remanent_state.get_file_retriever_options state
    | Some options -> state, options
  in
  let state, machine =
    match machine with
    | None ->
      Remanent_state.get_machine_to_access_gps state
    | Some machine -> state, machine
  in
  let state, port =
    match port with
    | None ->
      Remanent_state.get_port_to_access_gps state
    | Some port -> state, port
  in
  let state, input_repository =
    match input_repository with
    | None ->
      Remanent_state.get_repository_to_access_gps
        state
    | Some rep -> state, rep
  in
  let state, output_repository =
    match output_repository with
    | None ->
      begin
        let state, r1 =
          Remanent_state.get_local_repository state
        in
        let state, r2 =
            Remanent_state.get_repository_to_dump_gps_files
              state
        in
        match r1,r2 with
        | "","" -> state,""
        | "",x | x,"" -> state,x
        | x1,x2 -> state,Printf.sprintf "%s/%s" x1 x2
    end
    | Some rep -> state, rep
  in
  let promotion =
    match promotion with
    | None -> ""
    | Some x -> x
  in
  let state, prefix =
    match prefix with
    | None ->
      let state, bool =
        Remanent_state.get_store_gps_files_according_to_their_promotions state
      in
      state, if bool then promotion else ""
    | Some prefix -> state, prefix
  in
  let state, output_file_name =
    match file_name with
    | None ->
      let state,bool =
        Remanent_state.get_indicate_promotions_in_gps_file_names state
      in
      state, (if promotion = "" && not bool
      then ""
      else promotion^"_")^lastname^"_"^firstname^".gps.csv"
    | Some file_name -> state, file_name
  in
  let state,timeout =
    match timeout with
    | None ->
      Remanent_state.get_file_retriever_time_out_in_second state
    | Some t -> state, t
  in
  let state, period =
    match checkoutperiod
    with
    | None ->
      Remanent_state.get_file_retriever_checking_period  state
    | Some t -> state,t
  in
  let output_file_name =
    Tools.remove_space_from_string output_file_name
  in
  let url =
    Printf.sprintf
      "http://%s:%s/%s/gps.pl?last=%s&first=%s"
      machine
      port
      input_repository
      lastname
      firstname
  in
  let output_repository =
    match output_repository,prefix  with
    | ".",prefix | "",prefix -> prefix
    | x,"" -> x
    | x1,x2 ->
      Printf.sprintf "%s/%s" x1 x2
  in
  let state, output_repository =
    Safe_sys.rec_mk_when_necessary
      __POS__
      state output_repository
  in
  match
    File_retriever.launch
      file_retriever ?user_name ?password ~options
      ?log_file ?log_repository
      ~url ~output_repository ~output_file_name ?timeout state
  with
  | state, 0 ->
    File_retriever.check
      ?log_file ?log_repository ~period ~output_repository ~output_file_name
      ?timeout
      file_retriever state
  | state, _ ->
    let () =
      Remanent_state.log
        state
        "The extraction of the GPS file for %s %s (%s) failed" firstname lastname promotion
    in
    Remanent_state.warn __POS__
      (Printf.sprintf "The extraction of the GPS file for %s %s (%s) failed" firstname lastname promotion)
      Exit
      state

  let get_student_file
      student_id
      ?file_retriever ?command_line_options ?machine ?port ?input_repository ?output_repository ?prefix ?timeout ?checkoutperiod
      ?file_name ?log_file ?log_repository
      ?user_name ?password
      state
    =
    let event_opt =
      Some
        (Profiling.Extract_gps_file
           (student_id.Public_data.firstname,
            student_id.Public_data.lastname))
    in
    let state =
      Remanent_state.open_event_opt
        event_opt
        state
    in
    let state =
      get_student_file
      student_id
      ?file_retriever ?command_line_options ?machine ?port ?input_repository ?output_repository ?prefix ?timeout ?checkoutperiod
      ?file_name ?log_file ?log_repository
      ?user_name ?password
      state
    in
    Remanent_state.close_event_opt
      event_opt
      state

type student_id =
  {
    lastname: string option;
    firstname: string option;
    promotion: string option
  }

let empty_student =
  {
    lastname = None ;
    firstname = None ;
    promotion = None}

let fun_ignore =
  (fun state _ x -> state, x)
let asso_list =
  [
    Public_data.Ignore, fun_ignore ;
    Public_data.Courriel, fun_ignore ;
    Public_data.Statut, fun_ignore ;
    Public_data.Origine, fun_ignore ;
    Public_data.Departement, fun_ignore ;
    Public_data.Contrat, fun_ignore ;
    Public_data.Recu, fun_ignore ;
    Public_data.Pers_id, fun_ignore ;
    Public_data.LastName,
    (fun state lastname (x,_) ->
       state, ({x with lastname},false));
    Public_data.FirstName,
    (fun state firstname (x,_) ->
       state, ({x with firstname},false));
    Public_data.Promo,
    (fun state promotion (x,allset) ->
        state, ({x with promotion},allset));
    ]
let get_students_list
    ?repository
    ?prefix
    ?file_name
    ?promotion
    state
  =
  let event_opt = Some (Profiling.Extract_gps_data_base) in
  let state =
    Remanent_state.open_event_opt
      event_opt
      state
  in
  let state, is_keyword, translate, action  =
    Keywords_handler.make
      state
      asso_list
  in
  let after_array_line
      _header state current_file current_file' _allset output =
    state, current_file, true, current_file'::output
  in
  let after_array _header (state:Remanent_state.t) current_file allset output =
    state, current_file, allset, output in
  let at_end_of_file state current_file allset output =
    if allset then
      state, output
    else
      state, current_file::output
  in
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
  let state, files_list =
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
  in
  let state, list =
    List.fold_left
      (fun (state, output) (file:string*string) ->
         Scan_csv_files.get_list_from_a_file
           is_keyword action translate
           after_array_line after_array at_end_of_file
           empty_student
           state file output)
      (state, []) files_list
  in
  let p promo promo' =
    match promo,promo' with
      Some x, Some y  -> x=y
    | None, _ | _, None -> true
  in
  let state, output =
    List.fold_left
      (fun (state, output) student ->
         if p promotion student.promotion
         then
           match student.firstname, student.lastname
           with
           | None, _ | _, None ->
             Remanent_state.warn_dft
               __POS__ "" Exit output state
           | Some firstname, Some lastname ->
             state,{Public_data.firstname = firstname ;
                    Public_data.lastname=lastname;
                    Public_data.promotion= student.promotion
                   }::output
         else
           state, output)
      (state,[]) list
  in
  let state =
    Remanent_state.close_event_opt
      event_opt
      state
  in
  state, output

let get_dated_repository state =
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
  let date_string_of_tm tm =
    Printf.sprintf "%0*d%0*d%0*d"
      4 (1900 + tm.Unix.tm_year)
      2 (1 + tm.Unix.tm_mon)
      2 tm.Unix.tm_mday
  in
  let date = date_string_of_tm (Unix.gmtime (Unix.time ())) in
  let state, current_dir = Safe_sys.getcwd __POS__ state in
  let state = Safe_sys.chdir __POS__ state rep in
  let state, f_exists =
    Safe_sys.file_exists __POS__ state date
  in
  let state =
    if f_exists then
      Safe_sys.command __POS__ state (Printf.sprintf "rm -rf %s" date)
    else
      state
  in
  let state = Safe_sys.command __POS__ state (Printf.sprintf "mkdir %s" date) in
  let state = Safe_sys.command __POS__ state "rm -rf courant" in
  let full_output_rep, full_courant =
    match rep with
    | "" -> date,"courant"
    | _ -> Printf.sprintf "%s/%s" rep date,
            Printf.sprintf "%s/courant" rep
  in
  let state =
    Safe_sys.command __POS__ state
      (Printf.sprintf "ln -sf %s %s" full_output_rep full_courant)
  in
  let state = Safe_sys.chdir __POS__ state current_dir in
  state, full_output_rep
