type 'a mandatory_field =
  {
    check:(Remanent_state.t -> 'a -> Remanent_state.t * bool);
    label: string
  }

let get_list_from_a_file
    ~strict
    get_csv
    automaton
    empty
    state  (rep,file) output =
  let is_keyword = automaton.Keywords_handler.is_keyword in
  let action = automaton.Keywords_handler.action in
  let of_interest= automaton.Keywords_handler.flush_required in
  let translate = automaton.Keywords_handler.translate in
  let shared_part = automaton.Keywords_handler.shared in
  let do_at_end_of_file = shared_part.Keywords_handler.do_at_end_of_file in
  let do_at_end_of_array = shared_part.Keywords_handler.do_at_end_of_array in
  let do_at_end_of_array_line =
    shared_part.Keywords_handler.do_at_end_of_array_line
  in
  let flush =
    shared_part.Keywords_handler.flush in
  let file =
    if rep = ""
    then
      file
    else
      Printf.sprintf "%s/%s" rep file
  in
  let state, csv_opt = get_csv file state in
  match csv_opt with
  | None -> state, []
  | Some csv ->
    let rec scan
        state
        current_line remaining_lines current_keyword filled current_file
        is_non_empty output =
      match current_line with
      | [] ->
        begin
          match remaining_lines with
          | [] ->
            let state, output =
              if is_non_empty
              then
                let state = Remanent_state.warn __POS__ "flush" Exit state in 
                flush state current_file output
              else
                let state = Remanent_state.warn __POS__ "ignored" Exit state in 
                state, output
            in
            do_at_end_of_file state current_file output
          | h::t ->
            let state = Remanent_state.warn __POS__ "Next line" Exit state in 
            if List.length
                (List.filter (fun s -> s <> "") h) > 1
            then
              let state, b =
                let rec aux state l =
                  match l with
                  | [] -> state, true
                  | h::q ->
                    let state, b =
                      if h="" then
                        state, true
                      else
                        is_keyword __POS__ state h
                    in
                    if b then
                      aux state q
                    else
                      state, false
                in
                aux state h
              in
              if b
              then
                let state, header_key, header =
                  List.fold_left
                    (fun (state, h_key, h) elt ->
                       if elt = ""
                       then
                         state,(Some Public_data.Ignore)::h_key,
                         (Some (fun a _ c -> a,c))::h
                       else
                         let state, action =
                         action __POS__ state elt
                       in
                       let state, key =
                         translate __POS__ state elt
                       in
                       state, key::h_key, action::h)
                    (state,[],[])
                    (List.rev h)
                in
                array_mode state header_key header t current_file output
              else
                scan
                  state h t current_keyword filled current_file is_non_empty output
            else
              scan
                state h t current_keyword filled current_file is_non_empty output

        end
      | h::t ->
        let state, b = is_keyword __POS__ state h in
        if b then
          let state, action =
            action __POS__ state h
          in
          let state, of_interest =
            of_interest __POS__ state h
          in
          let state, current_file =
            match action
            with
            | Some action ->
              action state None current_file
            | None -> state, current_file
          in
          scan
            state t remaining_lines (Some (action,of_interest)) false current_file
            is_non_empty
            output
        else
        if String.trim h = "" && filled then
        scan
          state
          t remaining_lines
          current_keyword filled current_file
          is_non_empty output
        else
          let state, current_file,is_non_empty,filled =
            match current_keyword with
            | None -> state, current_file, is_non_empty,filled
            | Some (None, None) ->
            let state =
              Remanent_state.warn
                __POS__
                (Format.sprintf "Action and of_interest fields are missing in %s" file )
                Exit
                state
            in
            state, current_file, is_non_empty,filled
            | Some (None,_) ->
              let state =
                Remanent_state.warn
                  __POS__
                  (Format.sprintf "Action field is missing in %s" file)
                  Exit
                  state
              in
              state, current_file, is_non_empty, filled
            | Some (_,None)->
              let state =
                Remanent_state.warn
                  __POS__
                  (Format.sprintf "Of interest field is missing in %s" file) Exit
                  state
              in
              state, current_file, is_non_empty, filled
            | Some (Some action,Some of_interest) ->
              let state, current_file =
                action state (Some h) current_file
              in
              let is_non_empty =
                is_non_empty || of_interest
              in
              state, current_file, is_non_empty, true
          in
          scan
            state
            t remaining_lines
            current_keyword filled current_file
            is_non_empty output
    and
      array_mode state header_key header
        remaining_lines current_file output  =
      match remaining_lines with
      | [] ->
        let state, current_file, output =
          do_at_end_of_array
            header_key state current_file output
        in
        scan
          state [] [] None false current_file false output
      | []::t ->
        array_mode state header_key header t current_file output
      | [x]::t when Tools.space_only x ->
        array_mode state header_key header t current_file output
      | h::t ->

        let rec aux state l =
          match l with
          | [] -> state, false
          | h::q ->
            let state, b =
              is_keyword __POS__ state h
            in
            if b then
              state, true
            else  aux state q
        in
        let state, b =
            if not strict then state, false
            else aux state h in
        if b
        then
          let state, current_file, output =
            do_at_end_of_array
              header_key state current_file output
          in
          scan
            state [] remaining_lines None false current_file false output
        else
          let rec aux state header data current_file =
            match header,data with
            | _, [] | [], _ ->
              let state = Remanent_state.warn __POS__ "Incomplete row" Exit state in 
              state, current_file
            | None::tk, _::td ->
              let state =
                Remanent_state.warn
                  __POS__
                  (Format.sprintf "Action field is missing in %s" file)
                  Exit
                  state
              in
              aux state tk td current_file
            | (Some hk)::tk, hd::td ->
              let state, current_file =
                hk state (Some hd) current_file in
              aux state tk td current_file
          in
          let state, current_file' =
            aux state header h current_file in
          let state, current_file, output =
            do_at_end_of_array_line
              header_key state current_file current_file' output
          in
          array_mode state header_key header t current_file output
    in
    scan state [] csv None false empty false output

let get_list
    ~strict
    ~get_csv ?debug
    ~repository ?prefix ?file_name ?automaton
    ~keywords_list ~all_fields ~fun_default ~keywords_of_interest
    ~at_end_of_array ~at_end_of_file ~at_end_of_array_line ~flush
    ~init_state
    state output =
  let state, files_list =
      Scan_repository.get_list_of_files
        ~repository ?prefix ?file_name state
    in
    match files_list with
      | [] -> state, automaton, output
      | _ ->
      let state, automaton  =
        match automaton with None ->
        Keywords_handler.make ?debug
          state
          {
            Keywords_handler.keywords = keywords_list ;
            Keywords_handler.all_fields ;
            Keywords_handler.default = fun_default ;
            Keywords_handler.of_interest = keywords_of_interest;
            Keywords_handler.shared_functions =
              {
                Keywords_handler.do_at_end_of_array = at_end_of_array;
                Keywords_handler.do_at_end_of_file = at_end_of_file;
                Keywords_handler.do_at_end_of_array_line = at_end_of_array_line;
                Keywords_handler.flush = flush ;
              }
          }
      | Some x -> state, x
      in
      let state, output = List.fold_left
        (fun (state, output) file ->
           let event = Some (Profiling.Scan_csv_files (fst file,snd file)) in
           let state = Remanent_state.open_event_opt event state in
           let _ =
             Format.printf
               "Scanning file: %s %s @." (fst file) (snd file)
           in
           let _ =
             Format.print_newline ()
           in
           let _ =
             Format.print_flush ()
           in
           let state, output =
              get_list_from_a_file
                  ~strict
                  get_csv
                  automaton
                  init_state
                  state file output
            in
            let state = Remanent_state.close_event_opt event state in
            state, output)
        (state, output) files_list
      in state, Some automaton, output

let unify_gen
    pos ~all_fields
    state elt elt' =
  let state, elt, list_incompatible, list_compatible =
    List.fold_left
      (fun
        (state,
         elt,
         list_incompatible,
         list_compatible)
        field ->
        match
          field.Keywords_handler.is_unifyable state elt elt'
        with
        | state, true ->
          let state, msg = field.Keywords_handler.label1 state elt in
          let state, elt_opt = field.Keywords_handler.unify state elt elt' in
          state,
          begin
            match elt_opt with
            | None -> elt
            | Some elt -> elt
          end,
          list_incompatible, List_of_string.add_opt msg list_compatible
        | state, false ->
          let state, msg = field.Keywords_handler.label2 state elt elt' in
          state, elt, List_of_string.add_opt msg list_incompatible, list_compatible
      )
      (state, elt, List_of_string.empty, List_of_string.empty)
      all_fields
  in
  if List_of_string.is_empty list_incompatible
  then
    state, elt
  else
    let msg =
      Format.sprintf
        "%s for %s"
        (List_of_string.to_string list_incompatible "is imcompatible" "are incompatible")
        (List_of_string.to_string list_compatible "" "")
    in
    Remanent_state.warn_dft
      pos
      msg
      Exit
      elt
      state


let collect_gen
    ?debug
    ~strict
    ?repository
    ?prefix
    ?file_name
    ?p
    ~get_csv
    ~compute_repository
    ~fun_default
    ~keywords_of_interest
    ~keywords_list
    ~init_state
    ~empty_elt
    ~add_elt
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
  =
  let unify =
    unify_gen ~all_fields
  in
  let add_elt = add_elt unify in
  let state, repository =
    match repository with
    | Some a -> state, Some a
    | None ->
      let state, a =
        compute_repository state
      in
      state, Some a
  in
  let state =
    Remanent_state.open_event_opt
      event_opt
      state
  in
  let p =
    match p with
    | None -> (fun _ -> true)
    | Some p -> p
  in
  let at_end_of_array_line
      _header state current_file current_file' output =
    let
      state, list_missing =
      List.fold_left
        (fun (state,list) mandatory_field ->
           match
             mandatory_field.check state current_file'
           with
           | state, true -> state,list
           | state, false ->
             state, List_of_string.add mandatory_field.label list)
        (state, List_of_string.empty)
        mandatory_fields
    in
    if p current_file'
    then
      if List_of_string.is_empty list_missing
      then
        let state = Remanent_state.warn __POS__ "EMPTY LIST_MISSING" Exit state in 
        state, current_file, current_file'::output
      else
      begin
        let state, list_known  =
          List.fold_left
            (fun (state, list_known) field ->
               match field.Keywords_handler.label_tmp
                       state current_file'
               with
               | state, Some x -> state,
                                  List_of_string.add x list_known
               | state, None -> state, list_known)
            (state, List_of_string.empty)
            all_fields
        in
        let known = List_of_string.to_string list_known "" "" in
        let missing = List_of_string.to_string list_missing "is missing" "are missing" in
        let msg =
          Format.sprintf "%s for %s in %s" missing known
                (match file_name with None -> "unknown" | Some x -> x)
        in
        let state =
          Remanent_state.warn
            __POS__
            msg
            Exit
            state
        in
        state, current_file, output
      end
  else
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
    if p current_file then
       state, current_file::output
    else state, output
  in
  let state, repository =
    match repository with
    | Some a -> state, a
    | None -> Remanent_state.get_monitoring_list_repository state
  in
  let all_fields_short =
    List.rev_map
      (Keywords_handler.shorten)
      (List.rev all_fields)
  in

  let state, _automaton, list =
    get_list ~get_csv ~strict ?debug
      ~keywords_of_interest ~all_fields:all_fields_short ~keywords_list
      ~fun_default
      ~at_end_of_array_line ~at_end_of_array ~at_end_of_file ~flush
      ~init_state
      state
      ~repository ?prefix ?file_name
      []
  in
  let state =
    List.fold_left
      (fun state elt ->
         let state, new_elt =
           List.fold_left
             (fun (state, new_elt) field ->
                field.Keywords_handler.update state elt new_elt)
             (state, empty_elt)
             all_fields
         in
         add_elt __POS__ new_elt state)
      state
      list
  in
  let state =
    Remanent_state.close_event_opt
      event_opt
      state
  in
  state
