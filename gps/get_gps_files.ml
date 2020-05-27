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
      "http://%s:%s/%s/gps.pl?last=\'%s\'&first=\'%s\'"
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
      file_retriever state, (output_repository,output_file_name)
  | state, i ->
    let () =
      Remanent_state.log
        state
        "The extraction of the GPS file for %s %s (%s) failed with error %i" firstname lastname promotion i
    in
    Remanent_state.warn __POS__
      (Printf.sprintf "The extraction of the GPS file for %s %s (%s) failed" firstname lastname promotion)
      Exit
      state, (output_repository,output_file_name)

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
    let state, output =
      get_student_file
        student_id
        ?file_retriever ?command_line_options ?machine ?port ?input_repository
        ?output_repository ?prefix ?timeout ?checkoutperiod
        ?file_name ?log_file ?log_repository
        ?user_name ?password
        state
    in
    Remanent_state.close_event_opt
      event_opt
      state, output

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
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Courriel ;
    Public_data.Statut ;
    Public_data.Origine ;
    Public_data.Departement ;
    Public_data.Contrat ;
    Public_data.Recu ;
    Public_data.Pers_id ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Promo ;
  ]
let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
  ]
let asso_list =
  [
    Public_data.LastName,
    (fun state lastname x ->
       state,
       let lastname =
         match lastname with
         | Some x when String.trim x = "" -> None
         | _ -> lastname
       in
       {x with lastname});
    Public_data.FirstName,
    (fun state firstname x ->
       state,
       let firstname =
        match firstname with
        | Some x when String.trim x = "" -> None
        | _ -> firstname
      in
      {x with firstname});
    Public_data.Promo,
    (fun state promotion x ->
        state, {x with promotion});
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
  let p promo promo' =
    match promo,promo' with
      Some x, Some y  -> x=y
    | None, _ | _, None -> true
  in
  let at_end_of_array_line
      _header state current_file current_file' output =
    match current_file'.firstname,current_file'.lastname with
    | None,None -> state, current_file, output
    | Some _, Some _ ->
      state, current_file, current_file'::output
    | Some x, None ->
      let promo =
        match current_file'.promotion with
        | None -> ""
        | Some x -> Format.sprintf " (PROMO %s)" x
      in
      let msg = Format.sprintf "Last name is missing for %s%s" x promo in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | None, Some x ->
      let promo =
        match current_file'.promotion with
        | None -> ""
        | Some x -> Format.sprintf " (PROMO %s)" x
      in
      let msg = Format.sprintf "First name is missing for %s%s" x promo in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
  in
  let at_end_of_array
      _header state current_file output =
    state, current_file, output
  in
  let at_end_of_file state _current_file output =
    state, output
  in
  let flush state current_file output =
    state, current_file::output
  in
  let state, list =
    Scan_csv_files.get_list
      ~keywords_of_interest ~asso_list ~keywords_list
      ~fun_default:fun_ignore
      ~at_end_of_array_line ~at_end_of_array ~at_end_of_file ~flush
      ~init_state:empty_student
      state
      ?repository ?prefix ?file_name
      []
  in
  let state, output =
    List.fold_left
      (fun (state, output) student ->
         if p promotion student.promotion
         then
           match student.firstname, student.lastname
           with
           | None, None -> state, output
           | None, Some x ->
             let promo =
               match promotion with
               | None -> ""
               | Some x -> Format.sprintf " (PROMO %s)" x
             in
             let msg =
               Format.sprintf "fistname is missing for %s%s" x promo
             in
             Remanent_state.warn_dft
             __POS__ msg Exit output state
           | Some x, None ->
             let promo =
               match promotion with
               | None -> ""
               | Some x -> Format.sprintf " (PROMO %s)" x
             in
             let msg =
               Format.sprintf "lastname is missing for %s%s" x promo
             in
           Remanent_state.warn_dft
           __POS__ msg Exit output state
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

let key = "Année académique"

let patch_student_csv state ?file_name csv =
  let rec aux state residual acc =
    match
      residual
    with
    | [] -> state, List.rev acc
    | h::t ->
      begin
        match h with
        | ["Nom";name] ->
          let list_of_string = String.split_on_char ' ' name in
          let rec aux' list firstname  =
            match list with
            | h::t ->
              if Tools.is_fully_capitalised h then
                List.rev firstname, list
              else
                aux' t (h::firstname)
            | [] ->
              List.rev firstname, []
          in
          let genre, firstname, lastname =
            match list_of_string with
            | [] | [_] -> "","",""
            | g::t ->
              let firstnamelist,lastnamelist = aux' t [] in
              g,
              String.concat " " firstnamelist,
              String.concat " " lastnamelist
          in
          if firstname = "" || lastname = ""
          then
            let state =
              Remanent_state.warn
                __POS__
                (Printf.sprintf
                   "Wrong full name in gps file %s"
                   (match file_name with None -> "" | Some x -> x))
                Exit
                state
            in
            aux state t (h::acc)
          else
            aux state t (["Genre";genre]::["Prénom";firstname]::["Nom";lastname]::["Nom Complet";name]::acc)
        | [s] when String.length s > 5 && (String.sub s 0 6) = "Année" ->
          let wordlist =
            String.split_on_char ' ' s
          in
          let wordlist =
            List.filter (fun s -> s<>"") wordlist
          in
          let state, annee =
            match wordlist with
              _::x::_ -> state, x
            | _ ->
              Remanent_state.warn_dft
                __POS__
                (Printf.sprintf
                   "Wrong academic year in gps file %s"
                   (match file_name with None -> "" | Some x -> x))
                Exit
                ""
                state
          in
          aux state t ([key;annee]::acc)
        | _ -> aux state t (h::acc)
      end
  in
  let state, csv = aux state csv [] in
  let rec step1 state residual output =
    match residual with
    | (k::_)::_ when k = key ->
      let output =
        List.fold_left
          (fun output elt -> elt::output)
          output
          residual
      in
      state, List.rev output
    | ("Semestre"::_)::_ ->
      step2 state residual output
    | h::t ->
      step1 state t (h::output)
    | [] ->
      let output =
        List.fold_left
          (fun output elt -> elt::output)
          output
          residual
      in
      let state =
        Remanent_state.warn
          __POS__
          "Ill-formed GPS file"
          Exit
          state
      in
      state, output
  and
    step2 state residual output =
    match residual with
    | (k::_)::_ when k = key ->
      let output =
        List.fold_left
          (fun output elt -> elt::output)
          output
          residual
      in
      state, List.rev output
    | _::t ->
      step2 state t output
    | [] ->
      let output =
        List.fold_left
          (fun output elt -> elt::output)
          output
          residual
      in
      let state =
        Remanent_state.warn
          __POS__
          "Ill-formed GPS file"
          Exit
          state
      in
      state, output
  in
  step1 state csv []

let patch_student_csv state ?file_name csv =
  let event_opt = Some (Profiling.Patch_gps_file file_name) in
  let state = Remanent_state.open_event_opt event_opt state in
  let state, output = patch_student_csv state ?file_name csv in
  let state = Remanent_state.close_event_opt event_opt state in
  state, output

let patch_student_file
    ~input
    ~output
    state
  =
  let state, separator =
    Remanent_state.get_csv_separator state
  in
  let (rep,file)=input in
  let file_name =
    if rep = ""
    then
      file
    else
      Printf.sprintf "%s/%s" rep file
  in
  let state, in_channel_opt =
    try
      state, Some (open_in file_name)
    with _ ->
      let () =
        Format.printf
          "Cannot open file %s@ "
          file
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s"  file_name)
        Exit
        state ,
      None
  in
  match in_channel_opt with
  | None -> state, None
  | Some in_channel ->
    let csv_channel =
      Csv.of_channel ?separator in_channel
    in
    let csv =
      Csv.input_all csv_channel
    in
    let state, csv =
      patch_student_csv state ~file_name csv
    in
    let () = close_in in_channel in
    let state =
      if input=output
      then
        Safe_sys.rm __POS__
          state file_name
      else
        state
    in
    let state, rep =
      Safe_sys.rec_mk_when_necessary __POS__
        state (fst output)
    in
    let file = snd output in
    let file =
      if rep = ""
      then
        file
      else
        Printf.sprintf "%s/%s" rep file
    in
    let state, output_channel_opt =
      try
        state, Some (open_out file)
      with _ ->
      let () =
        Format.printf
          "Cannot open file %s@ "
          file
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s"  file)
        Exit
        state ,
      None
    in
    match output_channel_opt with
    | None -> state, None
    | Some out ->
      let csv_channel =
        Csv.to_channel ?separator out
      in
      let () = Csv.output_all csv_channel csv in
      let () = close_out out in
      state, Some (rep, snd output)
