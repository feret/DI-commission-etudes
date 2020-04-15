let separator = Some ','

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
  let () =
    if Sys.file_exists
        output_repository
    then ()
    else
      let _ =
        Sys.command
          (Printf.sprintf "mkdir %s" output_repository)
      in ()
  in
  match
    File_retriever.launch
      file_retriever ?user_name ?password ~options
      ?log_file ?log_repository
      ~url ~output_repository ~output_file_name ?timeout
  with
  | 0 ->
    File_retriever.check
      ?log_file ?log_repository ~period ~output_repository ~output_file_name
      ?timeout
      file_retriever state
    | _ ->
    Remanent_state.warn __POS__
      (Printf.sprintf "The extraction of the GPS file for %s %s (%s) failed" firstname lastname promotion)
      Exit
      state

type student_id =
  {
    lastname: string option;
    firstname: string option;
    promotion: string option;
  }

let empty_student =
  {lastname = None ; firstname = None ; promotion = None}

type keywords = LastName | FirstName | Promo | Ignore

let is_keyword x =
  List.mem
    x
    [
      "NOM";"Prénom";"Courriel";"Promo";"Statut";"Origine";"Département";"Contrat";"Reçu";"Pers_id"
    ]

let keyword x =
  match x with
  | "NOM" -> LastName
  | "Prénom" -> FirstName
  | "Courriel" -> Ignore
  | "Promo" -> Promo
  | "Statut" -> Ignore
  | "Origine" -> Ignore
  | "Département" -> Ignore
  | "Contrat" -> Ignore
  | "Reçu" -> Ignore
  | "Pers_id" -> Ignore
  | _ -> Ignore

let get_students_from_a_file state (rep,file) output =
  let file =
    if rep = ""
    then
      file
    else
      Printf.sprintf "%s/%s" rep file
  in
  let in_channel_opt =
    try
      Some (open_in file)
    with _ ->
      let () =
        Format.printf
          "Cannot open file %s@ "
          file
      in
      None
  in
  match in_channel_opt with
  | None -> state, output
  | Some in_channel ->
      let in_channel =
        Csv.of_channel ?separator in_channel
      in
  let csv =
    Csv.input_all in_channel
  in
  let rec scan current_line remaining_lines current_keyword output current allset =
    match current_line with
    | [] ->
      begin
        match remaining_lines with
        | [] -> output
        | h::t ->
          if List.length h > 1
          && List.for_all is_keyword h
          then
            let h =
              List.rev_map
                keyword (List.rev h)
            in
            array_mode h t output current
          else
            scan
              h t current_keyword output current allset
      end
    | h::t ->
      if is_keyword h then
        scan
          t remaining_lines (Some (keyword h)) output current allset
      else
        let current,allset =
          match current_keyword with
          | Some Ignore | None -> current,allset
          | Some LastName -> {current with lastname = Some h},false
          | Some FirstName -> {current with firstname = Some h},false
          | Some Promo -> {current with promotion = Some h},allset
        in
        scan
          t remaining_lines None output current allset
  and
    array_mode header remaining_lines output current =
    match remaining_lines with
    | [] -> output
    | h::t ->
      if List.exists is_keyword h
      then
        scan
          [] remaining_lines None output current true
      else
        let rec aux header data current =
          match header,data with
          | _, [] | [], _ -> current
          | hk::tk, hd::td ->
            let current =
              match hk with
              | Ignore -> current
              | LastName -> {current with lastname = Some hd}
              | FirstName -> {current with firstname = Some hd}
              | Promo -> {current with promotion = Some hd}
            in
            aux tk td current
        in
        let current' = aux header h current in
        array_mode header t (current'::output) current
  in
  state, scan [] csv None output empty_student  true

let get_students_list
    ?repository
    ?prefix
    ?file_name
    ?promotion
    state
  =
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
  let files_list =
    match file_name with
    | Some file -> [repository,file]
    | None ->
      let rec explore files_list rep_to_explore =
        match rep_to_explore with
        | [] ->
          files_list
        | h::t ->
          let to_explore = Sys.readdir h in
          let files_list, rep_to_explore =
            Array.fold_left
              (fun (files_list,rep_list) file_to_explore
                ->
                  let complete =
                    Printf.sprintf
                      "%s/%s" h file_to_explore
                  in
                  if
                    Sys.is_directory complete
                  then
                    (files_list,complete::rep_list)
                  else
                    ((h,file_to_explore)::files_list,rep_list))
              (files_list,t)
              to_explore
          in
          explore files_list rep_to_explore
      in
      explore [] [repository]
  in
  let state, list =
    List.fold_left
      (fun (state, output) file ->
         get_students_from_a_file
           state file output)
      (state, []) files_list
  in
  let p promo promo' =
    match promo,promo' with
      Some x, Some y when x<>y -> false
    | (Some _ | None), (Some _ | None) -> true
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
    Printf.sprintf "04%d02%d02%d"
      (1900 + tm.Unix.tm_year)
      tm.Unix.tm_mon
      tm.Unix.tm_mday
  in
  let date = date_string_of_tm (Unix.gmtime (Unix.time ())) in
  let current_dir = Sys.getcwd () in
  let () = Sys.chdir rep in
  let _ =
    if Sys.file_exists date then
      let _ = Sys.command (Printf.sprintf "rm -rf %s" date)
      in ()
  in
  let _ = Sys.command (Printf.sprintf "mkdir %s" date) in
  let _ = Sys.command "rm -rf courant" in
  let full_output_rep, full_courant =
    match rep with
    | "" -> date,"courant"
    | _ -> Printf.sprintf "%s/%s" rep date,
            Printf.sprintf "%s/courant" rep
  in
  let _ =
    Sys.command
      (Printf.sprintf "ln -sf %s %s" full_output_rep full_courant)
  in
  let () = Sys.chdir current_dir in
  state, full_output_rep
