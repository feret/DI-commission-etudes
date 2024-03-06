type dpt = Maths | PE | DRI | PHYS | CHIMIE | GEOSCIENCES
type access_type =
       GPS of dpt option | Backup | Preempt | Warn

type mode =
  {
    access_type: access_type;
    avec_accent_sur_le_nom: bool;
    avec_accent_sur_le_prenom: bool;
  }

let string_of_dpt_opt =
  function
  | None -> ""
  | Some Maths -> "&dept=maths"
  | Some PE -> "&dept=pe"
  | Some DRI -> "&dept=dri"
  | Some PHYS -> "&dept=phys"
  | Some CHIMIE -> "&dept=chim"
  | Some GEOSCIENCES -> "&dept=5"

let profiling_label_of_dpt_opt =
  function
  | None -> None
  | Some Maths -> Some "DMA"
  | Some PE -> Some "PE"
  | Some DRI -> Some "DRI"
  | Some PHYS -> Some "PHYS"
  | Some CHIMIE -> Some "CHIMIE"
  | Some GEOSCIENCES -> Some "GSC"

let build_output
    pos ~has_promo
    ~f_firstname ~f_lastname student_id ?prefix ?output_repository ?output_file_name state =
  let firstname = student_id.Public_data.firstname in
  let lastname = student_id.Public_data.lastname in
  let promotion = student_id.Public_data.promotion in
  let get_repository =
    Remanent_state.get_repository_to_dump_gps_files ?output_repository
  in
  let get_store_according_promotion =
    Remanent_state.get_store_output_according_to_their_promotions
  in
  let get_indicate_promotions_in_file_names =
    Remanent_state.get_indicate_promotions_in_gps_file_names
  in
  let rec_mk_when_necessary =
    Safe_sys.rec_mk_when_necessary
  in
  Tools.build_output
    pos
    ~has_promo
    ~get_repository
    ~get_store_according_promotion
    ~get_indicate_promotions_in_file_names
    ~rec_mk_when_necessary
    ~f_firstname ~f_lastname ~firstname ~lastname ~promotion
    ?prefix ?output_repository ?output_file_name state

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
    ~f_firstname ~f_lastname student_id ?dpt
    ?file_retriever ?command_line_options ?machine ?port ?input_repository ?output_repository ?prefix ?timeout ?checkoutperiod
    ?output_file_name ?log_file ?log_repository
    ?user_name ?password
    state
  =
  let firstname =
    f_firstname student_id.Public_data.firstname
  in
  let lastname =
    f_lastname  student_id.Public_data.lastname
  in
  let promotion = student_id.Public_data.promotion in
  let promotion = Tools.unsome_string promotion in
  let event_opt =
    Some
      (Profiling.Extract_gps_file_from_database
        (firstname,lastname,profiling_label_of_dpt_opt dpt))
  in
  let state = Remanent_state.open_event_opt event_opt state in
  let state, output =
    if try int_of_string promotion < 2023 with _ -> true
    then
      begin
        let state, b = Remanent_state.get_file_retriever_skip state in
        if b then
          let () =
            Remanent_state.log
              state
              "The extraction of the GPS file for %s %s (%s) failed (too many access errors)" firstname lastname promotion
          in
          Remanent_state.warn __POS__
            (Printf.sprintf
              "The extraction of the GPS file for %s %s (%s) failed (too many access errors)" firstname lastname promotion)
            Exit
            state, None
        else
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
                  let state, x =
                      Remanent_state.get_file_retriever_log_repository state in
                  state, Some x
              | Some x -> state, Some x
          in
          let state, file_retriever =
            match file_retriever with
              | None -> Remanent_state.get_file_retriever state
              | Some wget -> state, wget
          in
          let state, options =
            match command_line_options with
              | None ->
                Remanent_state.get_file_retriever_options
                  ~more_options:Remanent_state.get_gps_access_options
                  state
              | Some options -> state, options
          in
          let state, machine =
            match machine with
              | None -> Remanent_state.get_machine_to_access_gps state
              | Some machine -> state, machine
          in
          let state, port =
            match port with
              | None -> Remanent_state.get_port_to_access_gps state
              | Some port -> state, port
          in
          let state, input_repository =
            match input_repository with
              | None -> Remanent_state.get_repository_to_access_gps state
              | Some rep -> state, rep
          in
          let state,timeout =
            match timeout with
              | None -> Remanent_state.get_file_retriever_time_out_in_second state
              | Some t -> state, t
          in
          let state, period =
            match
              checkoutperiod
            with
            | None -> Remanent_state.get_file_retriever_checking_period  state
            | Some t -> state,t
          in
          let url =
            Printf.sprintf
              "http://%s:%s/%s/gps.pl?last=\'%s\'&first=\'%s\'%s"
              machine
              port
              input_repository
              lastname
              firstname
              (string_of_dpt_opt dpt)
          in
          let state, output_repository, output_file_name =
            build_output
              __POS__
              ~has_promo:false
              ~f_firstname:(fun x -> x)
              ~f_lastname:(fun x -> x)
              ~extension:".gps.csv"
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
                      "The extraction of the GPS file for %s %s (%s) failed with error  %i" firstname lastname promotion i
                in
                Remanent_state.warn __POS__
                  (Printf.sprintf
                    "The extraction of the GPS file for %s %s (%s) failed" firstname lastname promotion)
                  Exit
                  state, None
          in
          let state = Remanent_state.close_event_opt event_opt state in
          state, output
      end
    else
      begin
        let state, separator =
          Remanent_state.get_csv_separator state
        in
        let state, output_repository, output_file_name =
            build_output
              __POS__
              ~has_promo:false
              ~f_firstname:(fun x -> x)
              ~f_lastname:(fun x -> x)
              ~extension:".gps.csv"
              student_id ?prefix ?output_repository ?output_file_name state
        in
        let csv =
          [
            ["Prénom";student_id.Public_data.firstname];
            ["Nom";student_id.Public_data.lastname];
            ["Année";Tools.unsome_string student_id.Public_data.promotion]
          ]
        in
        let file = output_file_name in
        let file =
          if output_repository = ""
          then
            file
          else
            Printf.sprintf "%s/%s" output_repository file
        in
        let state, output_channel_opt =
          try
            state, Some (open_out file)
          with exn ->
            let msg = Printexc.to_string exn in
            let () =
              Format.printf
                "Cannot open file %s (%s)@ "
                file
                msg
            in
            Remanent_state.warn
                __POS__
                (Format.sprintf "Cannot open file %s (%s)"  file msg)
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
            state, Some (output_repository, output_file_name)
      end
    in
    let state = Remanent_state.close_event_opt event_opt state in
    state, output



let copy
  ?save
  ~f_firstname ~f_lastname g student_id
  ?prefix ?output_repository ?output_file_name backup_rep state =
  let firstname =
    f_firstname  student_id.Public_data.firstname
  in
  let lastname =
    f_lastname student_id.Public_data.lastname
  in
  let event_opt =
    Some
      (g (firstname,lastname))
  in
  let state = Remanent_state.open_event_opt event_opt state in
  let promotion = student_id.Public_data.promotion in
  let promotion = Tools.unsome_string promotion in
  let state, output_repository, output_file_name =
    build_output
      __POS__
      ~has_promo:false
      ~f_firstname:(fun x -> x)
      ~f_lastname:(fun x -> x)
      student_id ?prefix ?output_repository
      ~extension:".gps.csv"
      ?output_file_name state
  in
  let state, _, input_file_name =
    build_output
      __POS__
      ~has_promo:true
      ~f_firstname ~f_lastname student_id ?prefix
      ~output_repository ~output_file_name
      ~extension:".gps.csv" state
  in
  let state, rep = backup_rep state in
  let state, prefix =
    match prefix with
    | None ->
      let state, bool =
        Remanent_state.get_store_output_according_to_their_promotions          state
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
  let state, output_repository =
      Safe_sys.rec_mk_when_necessary
          __POS__
          state
          output_repository
  in
  let output =
    match output_repository with
    | "" -> output_file_name
    | _ ->
      Printf.sprintf "%s/%s"
        output_repository output_file_name
  in
  let input, output =
    match save with
    | None | Some false -> input, output
    | Some true -> output, input
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
  let f_firstname =
    if mode.avec_accent_sur_le_prenom then
      Special_char.uppercase
    else
      (fun x -> Special_char.uppercase (Special_char.correct_string (Special_char.remove_simple_quote x)))
  in
  let f_lastname =
    if mode.avec_accent_sur_le_nom then
      Special_char.uppercase
    else
      (fun x -> Special_char.uppercase (Special_char.correct_string (Special_char.remove_simple_quote x)))
  in
  let state, output =
    match mode.access_type with
    | GPS dpt ->
      get_student_file_gen
        ~f_firstname ~f_lastname ?dpt student_id
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
        ~f_firstname ~f_lastname
        (fun (a,b) ->
           Profiling.Extract_gps_file_from_backup_files (a,b))
        student_id ?prefix ?output_repository ?output_file_name
        backup_rep state
    | Preempt ->
      let backup_rep =
        Remanent_state.get_repository_for_handmade_gps_files
      in
      copy
        ~f_firstname ~f_lastname
        (fun (a,b) ->
           Profiling.Extract_gps_file_from_handmade_files (a,b))
        student_id ?prefix ?output_repository ?output_file_name backup_rep state
    | Warn ->
      let state = Remanent_state.file_retriever_fail state in
      let state, current_year =
        Remanent_state.get_current_academic_year state
      in
      let state, l =
        Remanent_state.get_mentoring_list
          ~year:current_year
          state
      in
      let s string =
        Special_char.lowercase
          (Special_char.correct_string_txt
             (String.trim string))
      in
      let state, main_dpt =
        Remanent_state.get_main_dpt state
      in
      let state =
        List.fold_left
          (fun state elt ->
             if
                s student_id.Public_data.firstname
                    = s elt.Public_data.prenom_de_l_etudiant &&
                s student_id.Public_data.lastname
                    =
                    s elt.Public_data.nom_de_l_etudiant
             then
             Remanent_state.Collector_mentors.add
               state
               {Public_data.mentor_attribution_year =
                  elt.Public_data.annee_academique ;
                 Public_data.mentor_gender =
                   (match
                     elt.Public_data.genre_du_tuteur
                   with
                     None -> Public_data.Unknown
                   | Some x -> x);
                 Public_data.mentor_lastname =
                   Tools.unsome_string
                     elt.Public_data.nom_du_tuteur;
                 Public_data.mentor_firstname =
                   Tools.unsome_string
                     elt.Public_data.prenom_du_tuteur;
                Public_data.mentor_email =
                  Tools.unsome_string
                    elt.Public_data.courriel_du_tuteur;
                Public_data.mentor_academic_year =
                   elt.Public_data.annee_academique;
                 Public_data.mentor_student_promo = current_year ;
                 Public_data.mentor_student_gender =
                   Public_data.Unknown ;
                 Public_data.mentor_student_lastname = elt.Public_data.nom_de_l_etudiant ;
                 Public_data.mentor_student_firstname = elt.Public_data.prenom_de_l_etudiant ;
                Public_data.mentor_student_dpt = main_dpt ;
                Public_data.mentor_secondary = elt.Public_data.secondaire;
               } else state)
          state
          l
    in
    let state =
        Remanent_state.Gps_server_faillures.add
          state
          {
            Public_data.student_firstname_report =
              student_id.Public_data.firstname ;
            Public_data.student_lastname_report =
              student_id.Public_data.lastname ;
            Public_data.student_promo_report =
              match
                student_id.Public_data.promotion
              with
              | None -> ""
              | Some a -> a;
          }
      in state, None
  in
  match output with
  | None -> state, None
  | Some (output_repository,output_file_name)->
    check ~output_repository ~output_file_name state

let try_get_student_file
      mode student_id
      ?file_retriever ?command_line_options ?machine ?port
      ?input_repository ?output_repository ?prefix ?timeout
      ?checkoutperiod
      ?output_file_name ?log_file ?log_repository
      ?user_name ?password
      state =
  let state, output =
      try_get_student_file
        mode student_id
        ?file_retriever ?command_line_options ?machine ?port
        ?input_repository ?output_repository ?prefix ?timeout
        ?checkoutperiod
        ?output_file_name ?log_file ?log_repository
        ?user_name ?password
        state
  in
  match mode.access_type, output with
        | GPS _, Some _ ->
          let backup_rep =
            Remanent_state.get_repository_for_backup_gps_files
          in
          let f_firstname =
            if mode.avec_accent_sur_le_prenom then
              Special_char.uppercase
            else
              (fun x -> Special_char.uppercase (Special_char.correct_string x))
          in
          let f_lastname =
            if mode.avec_accent_sur_le_nom then
              Special_char.uppercase
            else
              (fun x -> Special_char.uppercase (Special_char.correct_string x))
          in
          let state,_ =
            copy
              ~save:true
              ~f_firstname ~f_lastname
              (fun (a,b) -> Profiling.Extract_gps_file_from_backup_files (a,b))
              student_id ?prefix ?output_repository ?output_file_name
              backup_rep state
          in
          state, output
        | _, None | Backup, _ | Preempt, _ | Warn, _-> state, output



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

let add_to_list simplified_firstname simplified_lastname access_type l =
  let firstname_list =
    if simplified_firstname then
      [true]
    else
      [true;false]
  in
  let lastname_list =
    if simplified_lastname then
      [true]
    else
      [true;false]
  in
  List.fold_left
    (fun l avec_accent_sur_le_nom ->
       List.fold_left
         (fun l avec_accent_sur_le_prenom ->
            {access_type;
             avec_accent_sur_le_nom; avec_accent_sur_le_prenom}::l)
         l
         firstname_list
    )
    l
    lastname_list

let modelist_di_gen b1 b2 =
  add_to_list b1 b2 Preempt
    (add_to_list b1 b2 (GPS None)
       (add_to_list b1 b2 (GPS (Some Maths))
          (add_to_list b1 b2 (GPS (Some PHYS))
            (add_to_list b1 b2 (GPS (Some DRI))
              (add_to_list b1 b2 (GPS (Some PE))
                  (add_to_list true true Warn
                    (add_to_list b1 b2 Backup [])))))))

let modelist_di_true_true =
  modelist_di_gen true true
let modelist_di_true_false =
  modelist_di_gen true false
let modelist_di_false_true =
  modelist_di_gen false true
let modelist_di_false_false =
  modelist_di_gen false false


  let modelist_helisa_gen b1 b2 =
    add_to_list b1 b2 Preempt
      (add_to_list b1 b2 Backup [])

  let modelist_helisa_true_true =
    modelist_helisa_gen true true
  let modelist_helisa_true_false =
    modelist_helisa_gen true false
  let modelist_helisa_false_true =
    modelist_helisa_gen false true
  let modelist_helisa_false_false =
    modelist_helisa_gen false false

let modelist_dma_gen b1 b2 =
  add_to_list b1 b2 Preempt
    (add_to_list b1 b2 (GPS (Some Maths))
        (add_to_list b1 b2 (GPS None)
          (add_to_list b1 b2 (GPS (Some PHYS))
            (add_to_list b1 b2 (GPS (Some DRI))
              (add_to_list b1 b2 (GPS (Some PE))
                  (add_to_list true true Warn
                    (add_to_list b1 b2 Backup []
                    )))))))

  let modelist_dma_true_true =
    modelist_dma_gen true true
  let modelist_dma_true_false =
    modelist_dma_gen true false
  let modelist_dma_false_true =
    modelist_dma_gen false true
  let modelist_dma_false_false =
    modelist_dma_gen false false

let modelist_phys_gen b1 b2 =
  add_to_list b1 b2 Preempt
    (add_to_list b1 b2 (GPS (Some PHYS))
       (add_to_list b1 b2 (GPS (Some Maths))
          (add_to_list b1 b2 (GPS (Some DRI))
             (add_to_list b1 b2 (GPS (Some PE))
                (add_to_list true true Warn
                   (add_to_list b1 b2 Backup []))))))

let modelist_phys_true_true =
  modelist_phys_gen true true
let modelist_phys_true_false =
  modelist_phys_gen true false
let modelist_phys_false_true =
  modelist_phys_gen false true
let modelist_phys_false_false =
  modelist_phys_gen false false


  let modelist_chimie_gen b1 b2 =
    add_to_list b1 b2 Preempt
      (add_to_list b1 b2 (GPS (Some CHIMIE))
         (add_to_list b1 b2 (GPS (Some Maths))
            (add_to_list b1 b2 (GPS (Some DRI))
               (add_to_list b1 b2 (GPS (Some PE))
                  (add_to_list true true Warn
                     (add_to_list b1 b2 Backup []))))))

let modelist_gsc_gen b1 b2 =
                       add_to_list b1 b2 Preempt
                         (add_to_list b1 b2 (GPS (Some GEOSCIENCES))
                            (add_to_list b1 b2 (GPS (Some Maths))
                               (add_to_list b1 b2 (GPS (Some DRI))
                                  (add_to_list b1 b2 (GPS (Some PE))
                                     (add_to_list true true Warn
                                        (add_to_list b1 b2 Backup []))))))

  let modelist_chimie_true_true =
    modelist_chimie_gen true true
  let modelist_chimie_true_false =
    modelist_chimie_gen true false
  let modelist_chimie_false_true =
    modelist_chimie_gen false true
  let modelist_chimie_false_false =
    modelist_chimie_gen false false


    let modelist_gsc_true_true =
      modelist_gsc_gen true true
    let modelist_gsc_true_false =
      modelist_gsc_gen true false
    let modelist_gsc_false_true =
      modelist_gsc_gen false true
    let modelist_gsc_false_false =
      modelist_gsc_gen false false

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
      event_opt state
  in
  let firstname = student_id.Public_data.firstname in
  let lastname = student_id.Public_data.lastname in
  let state, main_dpt = Remanent_state.get_main_dpt state in
  let promotion = student_id.Public_data.promotion in
  let helisa =
      try   match promotion with Some promotion -> int_of_string promotion > 2022
      | None -> false with _ -> false
  in

  let modelist =
    if helisa then
    match Special_char.remove_simple_quote (Special_char.remove_acute firstname) = firstname,
          Special_char.remove_simple_quote (Special_char.remove_acute lastname) = lastname
    with  true, true -> modelist_helisa_true_true
      | true, false -> modelist_helisa_true_false
      | false, false -> modelist_helisa_false_false
      | false, true -> modelist_helisa_false_true
    else
    match modelist with
    | Some modelist -> modelist
    | None ->
      begin
        match Special_char.remove_simple_quote (Special_char.remove_acute firstname) = firstname,
              Special_char.remove_simple_quote (Special_char.remove_acute lastname) = lastname,
              main_dpt
        with
        | true, true,
          (Public_data.DRI | Public_data.ARTS | Public_data.DI | Public_data.ENS
          | Public_data.IBENS | Public_data.ECO | Public_data.LILA | Public_data.DEC ) -> modelist_di_true_true
        | true, false,
          (Public_data.DRI | Public_data.ARTS | Public_data.DI | Public_data.ENS
          | Public_data.IBENS  | Public_data.ECO | Public_data.LILA | Public_data.DEC ) -> modelist_di_true_false
        | false, true,
          (Public_data.DRI | Public_data.ARTS | Public_data.DI | Public_data.ENS
          | Public_data.IBENS | Public_data.ECO | Public_data.LILA | Public_data.DEC )  -> modelist_di_false_true
        | false, false,
          (Public_data.DRI | Public_data.ARTS | Public_data.DI | Public_data.ENS
          | Public_data.IBENS | Public_data.ECO | Public_data.LILA | Public_data.DEC ) -> modelist_di_false_false
        | true, true, Public_data.DMA -> modelist_dma_true_true
        | true, false, Public_data.DMA -> modelist_dma_true_false
        | false, true, Public_data.DMA  -> modelist_dma_false_true
        | false, false, Public_data.DMA -> modelist_dma_false_false
        | true, true, Public_data.PHYS -> modelist_phys_true_true
        | true, false, Public_data.PHYS -> modelist_phys_true_false
        | false, true, Public_data.PHYS -> modelist_phys_false_true
        | false, false, Public_data.PHYS -> modelist_phys_false_false
        | true, true, Public_data.CHIMIE -> modelist_chimie_true_true
        | true, false, Public_data.CHIMIE -> modelist_chimie_true_false
        | false, true, Public_data.CHIMIE -> modelist_chimie_false_true
        | false, false, Public_data.CHIMIE -> modelist_chimie_false_false
        | true, true, Public_data.GEOSCIENCES -> modelist_gsc_true_true
        | true, false, Public_data.GEOSCIENCES-> modelist_gsc_true_false
        | false, true, Public_data.GEOSCIENCES -> modelist_gsc_false_true
        | false, false, Public_data.GEOSCIENCES-> modelist_gsc_false_false

      end
  in
  let state, output =
    get_student_file
      student_id
      ~modelist
      ?file_retriever ?command_line_options ?machine ?port ?input_repository
      ?output_repository ?prefix ?timeout ?checkoutperiod      ?output_file_name ?log_file ?log_repository
      ?user_name ?password
      state
  in
  Remanent_state.close_event_opt
    event_opt state,
  output

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
  Remanent_state.Student_ids.get_repository

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
    ~strict:true
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_student
    ~empty_elt:Public_data.empty_student_id
    ~add_elt:(fun _ _ a b -> Remanent_state.Student_ids.add b a)
    ~mandatory_fields
    ~all_fields
    ?event_opt
    ?p
    state

let key = "Année académique"

let patch_student_csv
    state
    ?firstname ?lastname ?file_name csv =
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
          let genre, firstname', lastname' =
            match list_of_string with
            | [] | [_] -> "","",""
            | g::t ->
              let firstnamelist,lastnamelist = aux' t [] in
              g,
              String.concat " " firstnamelist,
              String.concat " " lastnamelist
          in
          let firstname =
              match firstname with
              | None -> firstname'
              | Some a -> a
          in
          let lastname =
            match lastname with
            | None -> lastname'
            | Some a -> a
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
      state, List.rev output
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
      let state =
        Remanent_state.warn
          __POS__
          "Ill-formed GPS file"
          Exit
          state
      in
      state, List.rev output
  in
  step1 state csv []

let patch_student_csv state ?firstname ?lastname ?file_name csv =
  let event_opt = Some (Profiling.Patch_gps_file file_name) in
  let state = Remanent_state.open_event_opt event_opt state in
  let state, output = patch_student_csv state ?firstname ?lastname ?file_name csv in
  let state = Remanent_state.close_event_opt event_opt state in
  state, output

let patch_student_file
    ?firstname ?lastname ~input
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
      patch_student_csv
        state ?firstname ?lastname ~file_name csv
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
      with exn ->
      let msg = Printexc.to_string exn in
      let () =
        Format.printf
          "Cannot open file %s (%s)@ "
          file
          msg
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s (%s)"  file msg)
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
