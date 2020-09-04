let dump_missing_grades ?output_repository ?prefix ?file_name cmp s state  =
  let event_opt =
    Some (Profiling.Dump_missing_grades s)
  in
  let state =
    Remanent_state.open_event_opt event_opt state
  in
  let state, grades =
    Remanent_state.get_missing_grades state
  in
  let state, prefix =
    match prefix with
    | None -> state, ""
    | Some prefix -> state, prefix
  in
  let state, output_repository =
    match output_repository with
    | None ->
      Remanent_state.get_repository_to_dump_missing_grades state
    | Some rep -> state, rep
  in
  let state, output_file_name =
    match file_name with
    | None -> state, "missing_grades.html"
    | Some file_name -> state, file_name
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
  let file =
    if output_repository = ""
    then output_file_name
    else
      Printf.sprintf "%s/%s"
        output_repository output_file_name
  in
  let msg =
    Printf.sprintf "Missing_grade: file %s @ " file
  in
  let state =
    Remanent_state.warn
      __POS__
      msg
      Exit
      state
  in
  let state, output_channel_opt =
    try
      state, Some (open_out file)
    with _ ->
      let () =
        Format.printf
          "Cannot open file %s@."
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
  | None -> state
  | Some out ->
    let mode = Loggers.HTML_Tabular in
    let logger = Loggers.open_logger_from_channel ~mode out in
    let grades = List.sort cmp grades in
    let rec aux l prev output =
      match l with
      | h::t when h=prev -> aux t prev output
      | h::t -> aux t h (h::output)
      | [] -> List.rev output
    in
    let grades =
      match grades with
      | [] -> []
      | h::t ->
        aux t h [h]
    in
    let () =
      List.iter
        (fun grades ->
           let () =
             Loggers.open_row logger
           in
           let () =
             Loggers.print_cell logger
               grades.Public_data.missing_dpt
           in
           let year = grades.Public_data.missing_year in
           let year =
             try
               let year_int = int_of_string year in
               let year =
                 Format.sprintf
                   "%s -- %s"
                   year
                   (string_of_int (year_int +1))
               in
               year
             with _ -> year
           in
           let () =
             Loggers.print_cell logger
               year
           in
           let () =
             Loggers.print_cell logger
               grades.Public_data.missing_firstname
           in
           let () =
             Loggers.print_cell logger
               grades.Public_data.missing_lastname
           in
           let () =
             Loggers.print_cell logger
               grades.Public_data.missing_intitule
           in
           let () =
             Loggers.print_cell logger
               grades.Public_data.missing_teacher
           in
             let () =
             Loggers.close_row logger
           in
           Loggers.print_newline logger)
        grades
    in
    let () = Loggers.flush_logger logger in
    let () =
      match output_channel_opt with
      | Some chan -> close_out chan
      | None -> ()
    in
    let state =
      Remanent_state.close_event_opt event_opt state
    in
    state

let per_gen l a b =
  let rec aux l =
    match l with
    | [] -> compare a b
    | compare::t ->
      let cmp = compare a b in
      if cmp = 0
      then aux t
      else cmp
  in
  aux l

let lift_cmp f a b =
  compare (f a) (f b)

let per_dpt_class =
  per_gen
    [lift_cmp
      (fun a -> int_of_string a.Public_data.missing_dpt_indice);
     lift_cmp  (fun a -> a.Public_data.missing_intitule);
     lift_cmp  (fun a -> a.Public_data.missing_year);
     lift_cmp  (fun a -> a.Public_data.missing_lastname);
     lift_cmp  (fun a -> a.Public_data.missing_firstname);
  ]

let per_dpt_student =
  per_gen
    [
      lift_cmp (fun a -> int_of_string a.Public_data.missing_dpt_indice);
      lift_cmp (fun a -> a.Public_data.missing_lastname);
      lift_cmp (fun a -> a.Public_data.missing_firstname);
      lift_cmp (fun a -> a.Public_data.missing_intitule);
      lift_cmp (fun a -> a.Public_data.missing_year)]
