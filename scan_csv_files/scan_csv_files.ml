let get_list_from_a_file
    is_keyword action translate
    after_array_line after_array at_end_of_file
    empty
    state  (rep,file) output =
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
        allset output =
      match current_line with
      | [] ->
        begin
          match remaining_lines with
          | [] ->
            at_end_of_file state current_file allset output
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
                let state, h_fun, h_key =
                  List.fold_left
                    (fun (state, h_fun, h_key) elt ->
                       let state, action =
                         action __POS__ state elt
                       in
                       let state, (key:Public_data.keywords option) =
                         translate __POS__ state elt
                       in
                       state, action::h_fun, key::h_key)
                    (state,[],[])
                    (List.rev h)
                in
                array_mode state h_fun h_key t current_file allset output
              else
                scan
                  state h t current_keyword current_file allset output
            else
              scan
                state h t current_keyword current_file allset output

        end
      | h::t ->
        let state, b = is_keyword __POS__ state h in
        if b then
          let state, action =
            action __POS__ state h
          in
          scan
            state t remaining_lines (Some action) current_file allset output
        else
          let state, (current_file,allset) =
            match current_keyword with
            | None | Some None -> state, (current_file,allset)
            | Some (Some x) -> x state (Some h) (current_file,allset)
          in
          scan
            state t remaining_lines None current_file allset output
    and
      array_mode state header_fun header_key remaining_lines current_file allset output  =
      match remaining_lines with
      | [] ->
        state, output
      (*  | ([]|[""])::t -> array_mode state header_fun header_key t current_file allset output*)
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
          let state, current_file, allset, output =
            after_array header_key state current_file allset output
          in
          scan
            state [] remaining_lines None current_file allset output
        else
          let rec aux state header_fun data current_file allset =
            match header_fun,data with
            | _, [] | [], _ ->
              state, (current_file, allset)
            | None::tk, _::td ->
              aux state tk td current_file allset
            | (Some hk)::tk, hd::td ->
              let state, (current_file,allset) = hk state (Some hd) (current_file, allset) in
              aux state tk td current_file allset
          in
          let state, (current_file',_) = aux state header_fun h current_file allset in
          let state, current_file, allset, output =
            after_array_line header_key state current_file current_file' allset output
          in
          array_mode state header_fun header_key t current_file allset output
    in
    scan state [] csv None empty true output
