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
  let do_at_end_of_array_line = shared_part.Keywords_handler.do_at_end_of_array_line in
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
        state, output
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
    ?repository ?prefix ?file_name
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
        ?repository ?prefix ?file_name state
    in
      List.fold_left
        (fun (state, output) file ->
           get_list_from_a_file
             automaton
             init_state 
             state file output)
        (state, output) files_list
