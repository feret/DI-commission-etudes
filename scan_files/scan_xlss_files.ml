let get_csv file state =
  let ext = Tools.extension file in
  match ext with
    | "xlsx" -> state, Some (Xls_support.open_xlsx file)
    | "xls" ->
        begin
            let state, log_repository =
                  Remanent_state.get_file_retriever_log_repository state
              in
            let command = Format.sprintf "libreoffice --invisible --convert-to xlsx %s --outdir %s" file log_repository
        in
        let state = Safe_sys.command __POS__ state command in
        let _,basename = Tools.split_rep_filename file in
        let file = Format.sprintf "%s/%s.xlsx" log_repository basename in
        let output = Some (Xls_support.open_xlsx file) in
      (*  let state = Safe_sys.rm __POS__ state file in*)
        state, output
        end
    | _ -> state, None


let get_list_from_a_file
    ~strict
    automaton
    empty
    state  (rep,file) output =
  Scan_gen_files.get_list_from_a_file ~strict
      get_csv automaton empty state (rep,file) output

let get_list ?debug ~strict
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
               "Scanning file : %s %s @." (fst file) (snd file)
           in
           let _ =
             Format.print_newline ()
           in
           let _ =
             Format.print_flush ()
           in
           let state, output =
              Scan_gen_files.get_list_from_a_file
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

let collect_gen
    ?debug
    ~strict
    ?repository
    ?prefix
    ?file_name
    ?p
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
  Scan_gen_files.collect_gen
      ~get_csv
      ~strict
      ?debug
      ?repository
      ?prefix
      ?file_name
      ?p
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
