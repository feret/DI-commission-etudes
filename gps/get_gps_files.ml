type access_type =
       GPS | Backup | Preempt | Warn

type mode =
  {
    access_type: access_type;
    avec_accent: bool
  }

let build_output
    f student_id ?prefix ?output_repository ?output_file_name state =
  let firstname = f student_id.Public_data.firstname in
  let lastname = f student_id.Public_data.lastname in
  let promotion = student_id.Public_data.promotion in
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
    match output_file_name with
    | None ->
      let state,bool =
        Remanent_state.get_indicate_promotions_in_gps_file_names state
      in
      state, (if promotion = "" && not bool
      then ""
      else promotion^"_")^lastname^"_"^firstname^".gps.csv"
    | Some file_name -> state, file_name
  in
  let output_file_name =
    Tools.remove_space_from_string output_file_name
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
  state, output_repository, output_file_name

let get_file_name output_repository output_file_name =
  if output_repository = ""
  then
    output_file_name
  else
    Printf.sprintf "%s/%s"  output_repository output_file_name

let check ~output_repository ~output_file_name state =
  let file_name = get_file_name output_repository output_file_name in
  let state, in_channel_opt =
    try
      state, Some (open_in file_name)
    with _ ->
      let () =
        Format.printf
          "Cannot open file %s@ "
          file_name
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
    let state, separator =
      Remanent_state.get_csv_separator state
    in
    let csv_channel =
      Csv.of_channel ?separator in_channel
    in
    let csv =
      Csv.input_all csv_channel
    in
    let () = close_in in_channel in
    match csv with
    | _::_ ->
      state, Some (output_repository,output_file_name)
    | [] -> state, None



let get_student_file_gen
    f student_id
    ?file_retriever ?command_line_options ?machine ?port ?input_repository ?output_repository ?prefix ?timeout ?checkoutperiod
    ?output_file_name ?log_file ?log_repository
    ?user_name ?password
    state
  =
  let firstname = f student_id.Public_data.firstname in
  let lastname = f student_id.Public_data.lastname in
  let event_opt =
    Some
      (Profiling.Extract_gps_file_from_database (firstname,lastname))
  in
  let state = Remanent_state.open_event_opt event_opt state in
  let promotion = student_id.Public_data.promotion in
  let promotion = Tools.unsome_string promotion in
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
  let url =
    Printf.sprintf
      "http://%s:%s/%s/gps.pl?last=\'%s\'&first=\'%s\'"
      machine
      port
      input_repository
      lastname
      firstname
  in
  let state, output_repository, output_file_name =
    build_output
      (fun x -> x)
      student_id ?prefix ?output_repository ?output_file_name state
  in
  let state, output =
    match
      File_retriever.launch
        file_retriever ?user_name ?password ~options
        ?log_file ?log_repository
        ~url ~output_repository ~output_file_name ?timeout state
    with
    | state, 0 ->
        File_retriever.check
          ?log_file ?log_repository
          ~period ~output_repository
          ~output_file_name
          ?timeout
          file_retriever state,
        Some (output_repository,output_file_name)
    | state, i ->
      let () =
        Remanent_state.log
          state
          "The extraction of the GPS file for %s %s (%s) failed with  error %i" firstname lastname promotion i
      in
      Remanent_state.warn __POS__
        (Printf.sprintf
           "The extraction of the GPS file for %s %s (%s) failed" firstname lastname promotion)
        Exit
        state, None
  in
  let state = Remanent_state.close_event_opt event_opt state in
  state, output

let copy
    f g student_id ?prefix ?output_repository ?output_file_name backup_rep state =
  let firstname = f student_id.Public_data.firstname in
  let lastname = f student_id.Public_data.lastname in
  let event_opt =
    Some
      (g (firstname,lastname))
  in
  let state = Remanent_state.open_event_opt event_opt state in
  let promotion = student_id.Public_data.promotion in
  let promotion = Tools.unsome_string promotion in
  let state, output_repository, output_file_name =
    build_output
      (fun x -> x) student_id ?prefix ?output_repository ?output_file_name state
  in
  let state, _, input_file_name =
    build_output
      f student_id ?prefix ~output_repository ~output_file_name state
  in
  let state, rep =
    let state, r1 =
      Remanent_state.get_local_repository state
    in
    let state, r2 = backup_rep state in
    match r1,r2 with
    | "","" -> state,""
    | "",x | x,"" -> state,x
    | x1,x2 -> state,Printf.sprintf "%s/%s" x1 x2
  in
  let state, prefix =
    match prefix with
    | None ->
      let state, bool =
        Remanent_state.get_store_gps_files_according_to_their_promotions          state
      in
      state, if bool then promotion else ""
    | Some prefix -> state, prefix
  in
  let input_repository =
    match rep,prefix  with
    | ".",prefix | "",prefix -> prefix
    | x,"" -> x
    | x1,x2 ->
      Printf.sprintf "%s/%s" x1 x2
  in
  let input =
    match input_repository with
    | "" -> output_file_name
    | _ ->
      Printf.sprintf "%s/%s"
        input_repository input_file_name
  in
  let output =
    match output_repository with
    | "" -> output_file_name
    | _ ->
      Printf.sprintf "%s/%s"
        output_repository output_file_name
  in
  let state, b =
    Safe_sys.file_exists __POS__ state input
  in
  let state, output =
    if b then
      let command =
        Printf.sprintf "cp %s %s" input output
      in
      let state = Safe_sys.command __POS__ state command in
      state, Some (output_repository, output_file_name)
    else state, None
  in
  let state = Remanent_state.close_event_opt event_opt state in
  state, output

let try_get_student_file
    mode student_id
    ?file_retriever ?command_line_options ?machine ?port
    ?input_repository ?output_repository ?prefix ?timeout
    ?checkoutperiod
    ?output_file_name ?log_file ?log_repository
    ?user_name ?password
    state =
  let f =
    if mode.avec_accent then
      Special_char.uppercase
    else
      (fun x -> Special_char.uppercase (Special_char.correct_string x))
  in
  let state, output =
    match mode.access_type with
    | GPS ->
      get_student_file_gen
        f student_id
        ?file_retriever ?command_line_options ?machine ?port
        ?input_repository ?output_repository ?prefix ?timeout
        ?checkoutperiod
        ?output_file_name ?log_file ?log_repository
        ?user_name ?password state
    | Backup ->
      let backup_rep =
        Remanent_state.get_repository_for_backup_gps_files
      in
      copy
        f
        (fun (a,b) ->
           Profiling.Extract_gps_file_from_backup_files (a,b))
        student_id ?prefix ?output_repository ?output_file_name
        backup_rep state
    | Preempt ->
      let backup_rep =
        Remanent_state.get_repository_for_handmade_gps_files
      in
      copy
        f
        (fun (a,b) ->
           Profiling.Extract_gps_file_from_handmade_files (a,b))
        student_id ?prefix ?output_repository ?output_file_name backup_rep state
    | Warn ->
      let state, output_repository, output_file_name =
        build_output
          f student_id ?prefix ?output_repository ?output_file_name state
      in
      let output_file_name =
        get_file_name output_repository output_file_name
      in
      let state =
        Remanent_state.warn
          __POS__
          (Format.sprintf "File %s is empty, use backup version indeed"
             output_file_name)
          Exit
          state
      in state, None
  in
  match output with
  | None -> state, None
  | Some (output_repository,output_file_name)->
    check ~output_repository ~output_file_name state

let get_student_file
    student_id
    ~modelist
    ?file_retriever ?command_line_options ?machine ?port ?input_repository ?output_repository ?prefix ?timeout ?checkoutperiod
    ?output_file_name ?log_file ?log_repository
    ?user_name ?password
    state
  =
  let rec aux l state =
    match l with
    | [] ->
      let firstname = student_id.Public_data.firstname in
      let lastname = student_id.Public_data.lastname in
      let promotion = student_id.Public_data.promotion in
      let promotion = Tools.unsome_string promotion in
      Remanent_state.warn
        __POS__
        (Printf.sprintf
           "Cannot extract information about %s %s %s"
           firstname lastname
           (if promotion = "" then "" else Printf.sprintf "(%s)" promotion))
        Exit
        state,
      None
    | h::t ->
      match
        try_get_student_file
          h student_id
          ?file_retriever ?command_line_options ?machine ?port
          ?input_repository ?output_repository ?prefix ?timeout
          ?checkoutperiod
          ?output_file_name ?log_file ?log_repository
          ?user_name ?password
          state
      with
      | state, None -> aux t state
      | state, output -> state, output
  in
  aux modelist state

let add_to_list simplified access_type l =
  if simplified
  then
    {
      access_type ;
      avec_accent = true;
    }::l
  else
    {
      access_type ;
      avec_accent = true;
    }::
    {
      access_type ;
      avec_accent = false
    }::l

let modelist_gen b =
  add_to_list b Preempt
    (add_to_list b GPS
       (add_to_list true Warn
          (add_to_list b Backup [])))

let full_list = modelist_gen false
let simplified_list = modelist_gen true

let get_student_file
      student_id
      ?modelist
      ?file_retriever ?command_line_options ?machine ?port ?input_repository ?output_repository ?prefix ?timeout ?checkoutperiod
      ?output_file_name ?log_file ?log_repository
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
    let firstname = student_id.Public_data.firstname in
    let lastname = student_id.Public_data.lastname in
    let modelist =
      match modelist with
      | Some modelist -> modelist
      | None ->
        if Special_char.remove_acute firstname = firstname
        && Special_char.remove_acute lastname = lastname
        then
          simplified_list
        else
          full_list
    in
    let state, output =
      get_student_file
        student_id
        ~modelist
        ?file_retriever ?command_line_options ?machine ?port ?input_repository
        ?output_repository ?prefix ?timeout ?checkoutperiod
        ?output_file_name ?log_file ?log_repository
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

let event_opt = Some (Profiling.Extract_gps_data_base)

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_student Public_data.empty_student_id).Lift.safe
let lift_string_opt =
  (Lift.string empty_student Public_data.empty_student_id).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.lastname) "Student's family name";
    lift_pred (fun a -> a.firstname) "Student's first name";
  ]

let all_fields =
  let record_name = "student description" in
  [
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(fun state lastname x ->
       state,
       let lastname =
         match lastname with
         | Some x when String.trim x = "" -> None
         | _ -> lastname
       in
       {x with lastname})
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.lastname)
      ~set:(fun lastname a -> {a with Public_data.lastname})
      ~field_name:"student's last name"
      ~record_name
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(fun state firstname x ->
         state,
         let firstname =
           match firstname with
           | Some x when String.trim x = "" -> None
           | _ -> firstname
         in
         {x with firstname})
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.firstname)
      ~set:(fun firstname a -> {a with Public_data.firstname})
      ~field_name:"student's first name"
      ~record_name
      ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Promo
      ~set_tmp:(fun state promotion x ->
        state, {x with promotion})
      ~get_tmp:(fun a -> a.promotion)
      ~get:(fun a -> a.Public_data.promotion)
      ~set:(fun promotion a ->
          {a with Public_data.promotion})
      ~field_name:"Student's promotion"
      ~record_name
      ~pos:__POS__;
  ]

let compute_repository =
  Remanent_state.get_students_list_repository

let get_students_list
    ?repository
    ?prefix
    ?file_name
    ?promotion
    state
  =
  let p =
    match promotion with
    | None -> None
    | Some x ->
      Some (fun y -> match y.promotion with None -> true | Some y -> x = y)
  in
  Scan_csv_files.collect_gen
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_student
    ~empty_elt:Public_data.empty_student_id
    ~add_elt:Remanent_state.add_student
    ~mandatory_fields
    ~all_fields
    ?event_opt
    ?p
    state

let key = "Année académique"

let patch_student_csv state ?file_name csv =
  let rec aux state residual acc =
    match
      residual
    with
    | [] -> state, List.rev acc
    | (["Nom";_] as a)::(["Prénom";_] as b)::(["Genre";_] as c)::t ->
      aux state t (c::b::a::acc)
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
