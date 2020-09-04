type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

let dump_missing_grades ?output_repository ?prefix ?file_name cmp headers columns state  =
  let event_opt =
    Some Profiling.Dump_missing_grades
  in
  let state =
    Remanent_state.open_event_opt event_opt state
  in
  let state, grades =
    Remanent_state.get_missing_grades state
  in
  match grades with
  | [] -> state
  | _ ->
    let state, prefix =
      match prefix with
      | None -> state, ""
      | Some prefix -> state, prefix
    in
    let state, output_repository =
      match output_repository with
      | None ->
        Remanent_state.get_repository_to_dump_missing_grades
        state
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
    let state =
      match output_channel_opt with
      | None -> state
      | Some out ->
        let mode = Loggers.HTML in
        let logger = Loggers.open_logger_from_channel ~mode
            out in
        let extended_grades =
          Tools.prepare_report
            ~cmp
            ~headers:(List.rev_map (fun (_,_,a) -> a)
                        (List.rev headers))
            grades
        in
        let () =
          Tools.dump_report
            ~print_header:(Loggers.print_headers logger)
            ~open_row:(fun () -> Loggers.open_row logger)
            ~close_row:(fun () -> Loggers.close_row logger)
            ~print_cell:(Loggers.print_cell logger)
            ~close_array:(fun () -> Loggers.close_array logger)
            ~string_of_headers:(List.rev_map (fun (a,b,_) ->
                a,b) (List.rev headers))
            ~string_of_column:columns
            ~open_array:(Loggers.open_array logger)
            extended_grades in
        let () = Loggers.flush_logger logger in
        state
    in
    let () =
      match output_channel_opt with
      | Some chan -> close_out chan
      | None -> ()
    in
    let state =
      Remanent_state.close_event_opt event_opt state
    in
    state

let lift_cmp f a b =
  compare (f a) (f b)

let dump_per_dpt_student_year
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [lift_cmp (fun a -> int_of_string  a.Public_data.missing_dpt_indice);
     lift_cmp (fun a -> a.Public_data.missing_lastname);
     lift_cmp (fun a -> a.Public_data.missing_firstname);
     lift_cmp (fun a -> a.Public_data.missing_year)]
  in
  let headers =
    [
      "Département",(fun x -> x),(fun a -> a.Public_data.missing_dpt);
      "Etudiant",(fun x -> x),
      (fun a ->
         Printf.sprintf "%s %s"
           a.Public_data.missing_lastname
           a.Public_data.missing_firstname);
      "Année académique",
      (fun x -> x),
      (fun a ->
         let year = a.Public_data.missing_year in
         try
           let year =
             int_of_string year
           in
           Printf.sprintf "%i -- %i" year (year+1)
         with
           _ -> year)
    ] in
  let columns =
    [
      "CODE GPS",(fun a -> a.Public_data.missing_code_gps);
      "COURS",(fun a -> a.Public_data.missing_intitule);
      "ENSEIGNANT(E)",(fun a -> a.Public_data.missing_teacher)
    ] in
  dump_missing_grades ?output_repository ?prefix ?file_name cmp headers columns state

let dump_per_dpt_class_year
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [lift_cmp (fun a -> int_of_string  a.Public_data.missing_dpt_indice);
     lift_cmp (fun a -> a.Public_data.missing_intitule);
     lift_cmp (fun a -> a.Public_data.missing_year);
     lift_cmp (fun a -> a.Public_data.missing_lastname);
     lift_cmp (fun a -> a.Public_data.missing_firstname);]
  in
  let headers =
    [
      "Département",(fun x -> x),(fun a -> a.Public_data.missing_dpt);
      "Cours",(fun x -> x),
      (fun a ->
         match
           String.trim a.Public_data.missing_teacher,
           String.trim a.Public_data.missing_code_gps
         with
         | "","" -> a.Public_data.missing_intitule
         | s,"" | "",s ->
           Format.sprintf
             "%s (%s)"
             a.Public_data.missing_intitule s
         | s,s' ->
         Format.sprintf
           "%s (%s / %s)"
           a.Public_data.missing_intitule s s'
      );
      "Année académique",
      (fun x -> x),
      (fun a ->
         let year = a.Public_data.missing_year in
         try
           let year =
             int_of_string year
           in
           Printf.sprintf "%i -- %i" year (year+1)
         with
           _ -> year)
    ] in
  let columns =
    [
      "NOM",(fun a -> a.Public_data.missing_lastname);
      "PRÉNOM",(fun a -> a.Public_data.missing_firstname);
    ]
  in
  dump_missing_grades
    ?output_repository
    ?prefix
    ?file_name
    cmp headers columns state


let dump_per_student
    ?output_repository
    ?prefix
    ?file_name
    state =
  let cmp =
    [
      lift_cmp (fun a -> a.Public_data.missing_lastname);
      lift_cmp (fun a -> a.Public_data.missing_firstname);
      lift_cmp (fun a -> a.Public_data.missing_year);
      lift_cmp (fun a -> int_of_string  a.Public_data.missing_dpt_indice);
      lift_cmp (fun a -> a.Public_data.missing_intitule)
    ]
  in
  let headers =
    [
      "Étudiant",(fun x -> x),
      (fun a ->
         Printf.sprintf "%s %s"
           a.Public_data.missing_lastname
           a.Public_data.missing_firstname);
      "Année académique",
      (fun x -> x),
      (fun a ->
         let year = a.Public_data.missing_year in
         try
           let year =
             int_of_string year
           in
           Printf.sprintf "%i -- %i" year (year+1)
         with
           _ -> year)
    ]
  in
  let columns =
    [
      "DÉPARTEMENT",(fun a -> a.Public_data.missing_dpt);
      "CODE GPS",(fun a -> a.Public_data.missing_code_gps);
      "COURS",(fun a -> a.Public_data.missing_intitule);
      "ENSEIGNANT",(fun a -> a.Public_data.missing_teacher);
    ]
  in
  dump_missing_grades
    ?output_repository
    ?prefix
    ?file_name
    cmp headers columns state
