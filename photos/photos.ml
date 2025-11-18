let whereis
  ~firstname ~lastname ~promo state =
  Remanent_state.get_picture_potential_locations
    ~firstname ~lastname ~year:promo state

let is_avalailable
    ~firstname ~lastname ~promo state  =
  let state, list = whereis
      ~firstname ~lastname ~promo state
  in
  let rec aux state list =
    match list with
    | [] -> state, false
    | h::t ->
      let state, b = Safe_sys.file_exists __POS__ state h in
      if b then state, true
      else aux state t
  in aux state list

let simplify s =
  Special_char.lowercase
    (Special_char.correct_string_txt
       (String.trim s))

let grep_photo_address state file_name =
  let warn = Remanent_state.warn in
  let file_exists = Safe_sys.file_exists in
  let state, prefix =
    Remanent_state.get_url_prefix_for_photos state
  in
  let state, a =
    Tools.find_starting_with
      ~warn ~file_exists ~prefix ~between:'"'
      state file_name
  in
  match a with
  | Some a -> state, Some a
  | None ->
    let state, prefix =
      Remanent_state.get_rel_url_prefix_for_photos state
    in
    let state, correct =
      Remanent_state.get_correct_rel_url_prefix_for_photos state
    in
    let state, a =
      Tools.find_starting_with
        ~warn ~file_exists ~prefix ~between:'"'
        state file_name
    in
    match a with
    | None -> state, None
    | Some a -> state, Some (correct^a)

let check_url
    file_retriever ?user_name ?password ~options
    ?log_file ?log_repository
    ~url ~output_repository ~output_file_name
    ~period ?timeout ?tries
    firstname lastname promo
    state
  =
  let event_opt =
    Some
      (Profiling.Collect_record_from_url
         (firstname, lastname, promo, url))
  in
  let state =
    Remanent_state.open_event_opt
      event_opt state
  in
  let state, output =
    match
      File_retriever.launch
        file_retriever ?user_name ?password
        ~options
        ?log_file ?log_repository
        ~url ~output_repository ~output_file_name
        ?timeout ?tries 
        state
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
    Remanent_state.warn_and_log __POS__
      (Format.sprintf "access to %s failed with error %i" url i)
      Exit
      state,
    None
  in
  match output with
  | None ->
      state, false
  | Some (output_repository, output_file_name) ->
    let file_name =
      match output_repository with
      | "" -> output_file_name
      | _ -> Format.sprintf
               "%s/%s"
               output_repository output_file_name
    in
    let state, file_opt =
      grep_photo_address state file_name
    in
    let state, bool =
      match file_opt with
      | None -> state, false
      | Some file ->
        begin
          let event_opt =
            Some
              (Profiling.Collect_picture_from_url
                 (firstname, lastname, promo, url))
          in
          let state =
            Remanent_state.open_event_opt event_opt state
          in
          let url = file in
          let state, output_list =
          Remanent_state.get_picture_write_potential_locations
              ~firstname ~lastname ~year:promo state
          in
          let state, b =
            match output_list with
            | [] -> state, false
            | l ->
              List.fold_left
                (fun (state, b) a ->
                   let a = Tools.split_rep_filename a in
                   let ext = Tools.extension url in
                   let output_repository = fst a in
                   let output_file_name =
                     if ext = ""
                     then
                       snd a
                     else
                       Format.sprintf "%s.%s"
                         (Tools.basename (snd a))
                         ext
                   in
                   let filename =
                     match output_repository with
                     | "" -> output_file_name
                     | _ ->
                       Printf.sprintf
                         "%s/%s"
                         output_repository
                         output_file_name
                   in
                   match
                     File_retriever.launch
                       file_retriever ?user_name ?password
                       ~options
                       ?log_file ?log_repository
                       ~url ~output_repository
                       ~output_file_name
                       ?timeout ?tries state
                   with
                   | state, 0 ->
                     let state =
                       File_retriever.check
                         ?log_file ?log_repository
                         ~period ~output_repository
                         ~output_file_name
                         ?timeout 
                         file_retriever state
                     in
                     begin
                       try
                         let chan = open_in filename in
                         let n = in_channel_length chan in
                         let _ = close_in chan in
                         if n = 0
                         then
                           let state =
                             Safe_sys.rm
                               __POS__ state filename
                           in
                           Remanent_state.warn_and_log
                             __POS__
                             (Format.sprintf
                                "Access to url %s returned an empty file"
                                url)
                             Exit state,
                           false
                         else
                           state, true
                       with
                       | _ -> state, false
                     end
                   | state, i ->
                     let state =
                       let state, b =
                         Safe_sys.file_exists
                           __POS__ state filename
                       in
                       if b
                       then
                       Safe_sys.rm
                         __POS__ state filename
                     else
                       state
                   in
                 Remanent_state.warn_and_log
                     __POS__
                     (Format.sprintf
                        "Access to url %s failed with error %i"
                        url i)
                     Exit
                     state, b)
              (state, false)
              l
          in
          let state =
            Remanent_state.close_event_opt event_opt state
          in
          state, b
        end
    in
    let state =
      Remanent_state.close_event_opt
        event_opt state
    in
    state, bool

let fetch
    ?user_name ?password
    ?file_retriever ?command_line_options
    ?timeout ?tries ?checkoutperiod ?url_to_access_annuaire
    ?log_file ?log_repository ?tmp_repository ?tmp_file
    ~firstname ~lastname ~promo state =
  let event_opt =
    Some (Profiling.Collect_picture (firstname, lastname, promo))
  in
  let state = Remanent_state.open_event_opt event_opt state in
  let state, log_file =
    match log_file with
    | None ->
      let state, x =
        Remanent_state.get_file_retriever_log_file state
      in
      state, Some x
    | Some x -> state,Some x
  in
  let state, output_file_name =
    match tmp_file with
    | None ->
      let state, x =
        Remanent_state.get_file_retriever_annuaire_html_file state
      in
      state, x
    | Some x -> state, x
  in
  let state, output_repository =
    match tmp_repository with
    | None ->
        Remanent_state.get_file_retriever_annuaire_tmp_repository state
    | Some x -> state, x
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
    | None ->
      Remanent_state.get_file_retriever state
    | Some wget -> state, wget
  in
  let state, options =
    match command_line_options with
    | None ->
      Remanent_state.get_file_retriever_options
        ~more_options:Remanent_state.get_annuaire_access_options state
    | Some options -> state, options
  in
  let options = Format.sprintf "%s" options in
  let state,timeout =
    match timeout with
    | None ->
      Remanent_state.get_file_retriever_time_out_in_second state
    | Some t -> state, t
  in
   let state,tries =
    match tries with
    | None ->
      Remanent_state.get_file_retriever_number_of_tries state 
    | Some t -> state, t
  in
  let state, period =
    match checkoutperiod
    with
    | None ->
      Remanent_state.get_file_retriever_checking_period  state
    | Some t -> state,t
  in
  let lastname =
    Special_char.uppercase (simplify lastname)
  in
  let state, annuaire =
    match url_to_access_annuaire with
    | Some a -> state, a
    | None ->
      Remanent_state.get_url_to_access_annuaire state
  in
  let url =
    Format.sprintf
      "%sresultat.php?critere=%s"
      annuaire
      lastname
  in
  let url2 =
    Format.sprintf
      "%s+%s"
      url
      firstname
  in
  let url3 =
    Format.sprintf
      "%sresultat.php?critere=%s"
      annuaire
      (Special_char.correct_string_txt lastname)
  in
  let url4 =
    Format.sprintf
      "%s+%s"
      url3
      (Special_char.correct_string_txt firstname)
  in
  let add_url url list =
    if List.mem url list then list else url::list
  in
  let urls =
    List.fold_left
      (fun l elt -> add_url elt l)
      []
      [url4;url3;url2;url]
  in
  let rec aux state l successfull =
    match l with
    | [] ->
      begin
        state, successfull
      end
    | url::t ->
      begin
        let state, success =
          check_url
            file_retriever ?user_name ?password
            ~options
            ?log_file ?log_repository
            ~url ~output_repository ~output_file_name
            ~period ?timeout ?tries 
            firstname lastname promo
            state
        in
        aux state t (successfull || success)
      end
  in
  let state, b = aux state urls false in
  let state =
    if b then
      state
    else
      Remanent_state.warn
        __POS__
        (Printf.sprintf
           "The extraction of the picture for %s %s (%s) failed"
           firstname lastname promo)
        Exit
        state
  in
  Remanent_state.close_event_opt event_opt state



let get
    ?user_name ?password
    ~firstname ~lastname ?promo state =
  let state, promo =
    match promo with
    | Some promo -> state, Some promo
    | None ->
      match
        Remanent_state.get_promo ~firstname ~lastname state
      with
      | state, None ->
        Remanent_state.warn
          __POS__
          (Format.sprintf
             "Promotion year is not available for %s %s"
             firstname lastname)
          Exit
          state,
        None
      | state, Some promo -> state, Some promo
  in
  match promo with
  | None -> state, []
  | Some promo ->
      let state, b =
        is_avalailable ~firstname ~lastname ~promo state
      in
      let state =
        if b
        then state
        else
          fetch
            ?user_name ?password
            ~firstname ~lastname ~promo state
      in
      whereis ~firstname ~lastname ~promo state
