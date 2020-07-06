let get_list_from_a_file
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
  let state, separator =
    Remanent_state.get_csv_separator state
  in
  let file =
    if rep = ""
    then
      file
    else
      Printf.sprintf "%s/%s" rep file
  in
  let state, in_channel_opt =
    try
      state, Some (open_in file)
    with _ ->
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s"  file)
        Exit
        state ,
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
    let rec scan
        state
        current_line remaining_lines current_keyword current_file
        is_non_empty output =
      match current_line with
      | [] ->
        begin
          match remaining_lines with
          | [] ->
            let state, output =
              if is_non_empty
              then
                flush state current_file output
              else
                state, output
            in
            do_at_end_of_file state current_file output
          | h::t ->
            if List.length h > 1
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
                  state h t current_keyword current_file is_non_empty output
            else
              scan
                state h t current_keyword current_file is_non_empty output

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
          scan
            state t remaining_lines (Some (action,of_interest)) current_file
            is_non_empty
            output
        else
          let state, current_file,is_non_empty =
            match current_keyword with
            | None -> state, current_file, is_non_empty
            | Some (None, None) ->
            let state =
              Remanent_state.warn
                __POS__
                "Action and of_interest fields are missing"
                Exit
                state
            in
            state, current_file, is_non_empty
            | Some (None,_) ->
              let state =
                Remanent_state.warn
                  __POS__
                  "Action field is missing"
                  Exit
                  state
              in
              state, current_file, is_non_empty
            | Some (_,None)->
              let state =
                Remanent_state.warn
                  __POS__
                  "Of interest field is missing"
                  Exit
                  state
              in
              state, current_file, is_non_empty
            | Some (Some action,Some of_interest) ->
              let state, current_file =
                action state (Some h) current_file
              in
              let is_non_empty =
                is_non_empty || of_interest
              in
              state, current_file, is_non_empty
          in
          scan
            state t remaining_lines None current_file is_non_empty output
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
          state [] [] None current_file false output
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
            else
              aux state q
        in
        let state, b = aux state h in
        if b
        then
          let state, current_file, output =
            do_at_end_of_array
              header_key state current_file output
          in
          scan
            state [] remaining_lines None current_file false output
        else
          let rec aux state header data current_file =
            match header,data with
            | _, [] | [], _ ->
              state, current_file
            | None::tk, _::td ->
              let state =
                Remanent_state.warn
                  __POS__
                  "Action field is missing"
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
    scan state [] csv None empty false output

let get_list
    ~repository ?prefix ?file_name
    ~keywords_list ~asso_list ~fun_default ~keywords_of_interest
    ~at_end_of_array ~at_end_of_file ~at_end_of_array_line ~flush
    ~init_state
    state output =
    let state, automaton  =
      Keywords_handler.make
        state
        {
          Keywords_handler.keywords = keywords_list ;
          Keywords_handler.asso = asso_list ;
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
    in
    let state, files_list =
      Scan_repository.get_list_of_files
        ~repository ?prefix ?file_name state
    in
      List.fold_left
        (fun (state, output) file ->
           let _ =
             Format.printf
               "Scanning file : %s %s @." (fst file) (snd file)
           in
           let _ =
             Format.print_newline ()
           in
           let _ =
             Format.print_flush ()
           in
           get_list_from_a_file
             automaton
             init_state
             state file output)
        (state, output) files_list

let copy get set string_of pos msg =
  (fun state elt new_elt ->
  let state, x_opt = get state elt in
  match x_opt with
  | None ->
    Remanent_state.warn_dft
      pos
      msg
      Exit
      new_elt
      state
  | Some x ->
    set state new_elt x),
  (fun state a ->
     let state, x_opt = get state a in
     match x_opt with
     | None -> state, None
     | Some a -> string_of state a )

let copy_opt get set string_of =
  (fun state elt new_elt ->
     let state, x_opt = get state elt in
     let state, output =  set state new_elt x_opt in
     state, output ),
  (fun state a ->
     let state, x_opt = get state a in
     match x_opt with
     | None -> state, None
     | Some a ->
       let state, a =
         string_of state a
       in state, Some a)


let copy_safe f g h =
  copy
    (fun state a -> state, f a)
    (fun state a h -> state, g a h)
    (fun state a -> state, Some (h a))

let copy_opt_safe f g h =
  copy_opt
    (fun state a -> state, f a)
    (fun state a h -> state, g a h)
    (fun state a -> state, h a)

let collect_gen
    ?repository
    ?prefix
    ?file_name
    ?p
    ~compute_repository
    ~fun_default
    ~keywords_of_interest
    ~asso_list
    ~keywords_list
    ~init_state
    ~empty_elt
    ~add_elt
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
  =
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
      state, list_missing,last_missing =
      List.fold_left
        (fun (state,l,last) (get,message) ->
           match
             get state current_file'
           with
           | state, true -> state,l,last
           | state, false ->
             match last with
             | None ->
               state, l, Some message
             | Some x ->
               state, x::l, Some message)
        (state, [], None )
        mandatory_fields
    in
    match last_missing with
    | None ->
      if p current_file'
      then
        state, current_file, current_file'::output
      else
        state, current_file, output
    | Some last_missing ->
      begin
        let state, l_known, last  =
          List.fold_left
            (fun (state, l_known, last) (_,message) ->
               match message state current_file' with
               | state, Some x -> state,
                           begin
                             match last with None -> l_known
                                           | Some last -> last::l_known
                           end,
                           Some x
               | state, None -> state, l_known,last )
            (state, [], None)
            all_fields
        in
        let _ = Format.flush_str_formatter () in
        let _ =
          Format.pp_print_list
            ~pp_sep:(fun log () -> Format.fprintf log ", ")
            (fun log a -> Format.fprintf log "%s" a)
            Format.str_formatter
            l_known
        in
        let _ =
          match l_known, last with
          | _, None -> ()
          | [], Some x -> Format.fprintf Format.str_formatter "%s" x
          | _, Some x -> Format.fprintf Format.str_formatter ", and %s " x
        in
        let known = Format.flush_str_formatter () in
        let _ =
          Format.pp_print_list
            ~pp_sep:(fun log () -> Format.fprintf log ", ")
            (fun log a -> Format.fprintf log "%s" a)
            Format.str_formatter
            list_missing
        in
        let _ =
          match list_missing with
          | [] -> Format.fprintf Format.str_formatter "%s is missing" last_missing
          | _ -> Format.fprintf Format.str_formatter ", and %s are missing " last_missing
        in
        let missing = Format.flush_str_formatter () in
        let msg =
          Format.sprintf "%s for %s" missing known
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
  let state, repository =
    match repository with
    | Some a -> state, a
    | None -> Remanent_state.get_monitoring_list_repository state
  in
  let state, list =
    get_list
      ~keywords_of_interest ~asso_list ~keywords_list
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
             (fun (state, new_elt) (copy,_) ->
                copy state elt new_elt)
             (state, empty_elt)
             all_fields
         in
         add_elt __POS__ new_elt state)
      state
      list
  in
  state
