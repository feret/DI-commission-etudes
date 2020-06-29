
type dpt_id  =
  {
    acronyme: string option;
    full_name: string option;
    gerondif: string option;
    fontcolor: Color.color option;
    bgcolor: Color.color option;
  }

let empty_dpt =
  {
    acronyme = None;
    full_name = None;
    gerondif = None;
    fontcolor = None;
    bgcolor = None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Acronyme ;
    Public_data.FullName ;
    Public_data.Gerondif;
    Public_data.Couleur_du_fond;
    Public_data.Couleur_du_texte;
  ]

let keywords_of_interest =
  [
    Public_data.Acronyme ;
    Public_data.FullName ;
  ]

let asso_list =
  [
    Public_data.Acronyme,
    (fun state acronyme x ->
       state,
       let acronyme =
         match acronyme with
         | Some x when String.trim x = "" -> None
         | _ -> acronyme
       in
       {x with acronyme});
    Public_data.FullName,
    (fun state full_name x ->
       state,
       let full_name =
         match full_name with
         | Some x when String.trim x = "" -> None
         | _ -> full_name
       in
       {x with full_name});
    Public_data.Gerondif,
    (fun state gerondif x ->
       state,
       let gerondif =
         match gerondif with
         | Some x when String.trim x = "" -> None
         | _ -> gerondif
       in
       {x with gerondif});
    Public_data.Couleur_du_fond,
    (fun state bgcolor x ->
       let state, bgcolor =
         match bgcolor with
         | Some x when String.trim x = "" -> state, None
         | Some x ->
           begin
             match
               Color.color_of_string
                 (Special_char.lowercase
                     (Special_char.correct_string
                        (String.trim x)))
             with
             | Some x -> state, Some x
             | None ->
               let msg =
                 Format.sprintf
                   "Invalid color (%s)"
                   x
               in
               Remanent_state.warn_dft
                 __POS__
                 msg
                 Exit
                 None
                 state
           end
           | None -> state, None
       in
       state, {x with bgcolor});
    Public_data.Couleur_du_texte,
    (fun state fontcolor x ->
       let state, fontcolor =
         match fontcolor with
         | Some x when String.trim x = "" -> state, None
         | Some x ->
           begin
             match
               Color.color_of_string
                 (Special_char.lowercase
                    (Special_char.correct_string
                       (String.trim x)))
             with
             | Some x -> state, Some x
             | None ->
               let msg =
                 Format.sprintf
                   "Invalid color (%s)"
                   x
               in
               Remanent_state.warn_dft
                 __POS__
                 msg
                 Exit
                 None
                 state
           end
         | None -> state, None
       in
       state, {x with fontcolor});
  ]

let get_dpt
    ?repository
    ?prefix
    ?file_name
    state
  =
  let event_opt = Some (Profiling.Collect_departement) in
  let state =
    Remanent_state.open_event_opt
      event_opt
      state
  in
  let at_end_of_array_line
      _header state current_file current_file' output =
    match
      current_file'.full_name,
      current_file'.acronyme,
      current_file'.gerondif
    with
    | None,None,None -> state, current_file, output
    | Some _, Some _, Some _ ->
      state, current_file, current_file'::output
    | Some x, None, Some _ ->
      let msg = Format.sprintf "Acronyme name is missing for %s " x  in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | Some x, Some y, None  ->
      let msg =
        Format.sprintf "Gerundif is missing for %s (%s)"
          x y
      in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | None, Some x, Some _ ->
      let msg = Format.sprintf "Name is missing for %s" x  in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | Some x, None, None ->
      let msg = Format.sprintf "Acronym and gerundif are missing for %s" x in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | None, Some y, None  ->
      let msg =
        Format.sprintf "Complete name and gerundif are missing  for %s" y
      in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | None, None, Some y ->
      let msg = Format.sprintf "Complete name and acronyme are missing for gerundif %s" y in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
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
    state, current_file::output
  in
  let state, repository =
    match repository with
    | Some a -> state, a
    | None -> Remanent_state.get_departments_list_repository state
  in
  let state, list =
    Scan_csv_files.get_list
      ~keywords_of_interest ~asso_list ~keywords_list
      ~fun_default:fun_ignore
      ~at_end_of_array_line ~at_end_of_array ~at_end_of_file ~flush
      ~init_state:empty_dpt
      state
      ~repository ?prefix ?file_name
      []
  in
  let state =
    List.fold_left
      (fun state dpt ->
         match dpt.full_name,
               dpt.acronyme,
               dpt.gerondif
         with
         | Some full_name, Some acronyme, Some gerundif ->
           Remanent_state.add_dpt __POS__
             {Public_data.dpt_nom = full_name ;
              Public_data.dpt_acronyme = acronyme ;
              Public_data.dpt_gerundif = gerundif ;
              Public_data.dpt_bg_color = dpt.bgcolor;
              Public_data.dpt_font_color = dpt.fontcolor
             }
             state
         | None, None, None -> state
         | Some x, None, Some _ ->
           let msg =
             Format.sprintf
               "Acronyme is missing for %s"
               x
           in
           Remanent_state.warn
             __POS__
             msg
             Exit
             state
         | Some x, Some _, None  ->
             let msg =
               Format.sprintf
                 "Gerund is missing for %s"
                 x
             in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, Some x, Some _ ->
             let msg =
               Format.sprintf "Complete name is missing for %s" x in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | Some x, None, None ->
             let msg = Format.sprintf "Acronyme and gerund are missing for %s" x in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, Some y, None  ->
             let msg =
               Format.sprintf "Complete name and gerund are missing for %s" y
             in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, None, Some y ->
             let msg = Format.sprintf "Complete name and acronyme for the departement %s" y in
              Remanent_state.warn
               __POS__
               msg
               Exit
               state
      )
      state list
  in
  let state =
    Remanent_state.close_event_opt
      event_opt
      state
  in
  state


type program_id  =
  {
    dpt_acronym: string option;
    code_gps: string option;
    level: string option;
    intitule: string option;
  }

let empty_program =
  {
    dpt_acronym = None;
    code_gps = None;
    level = None;
    intitule = None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Departement ;
    Public_data.Code_gps;
    Public_data.Niveau ;
    Public_data.Intitule;
  ]

let keywords_of_interest =
  [
    Public_data.Departement ;
    Public_data.Code_gps;
  ]

let asso_list =
  [
    Public_data.Departement,
    (fun state dpt_acronym x ->
       state,
       let dpt_acronym =
         match dpt_acronym with
         | Some x when String.trim x = "" -> None
         | _ -> dpt_acronym
       in
       {x with dpt_acronym});
    Public_data.Code_gps,
       (fun state code_gps x ->
          state,
          let code_gps =
            match code_gps with
            | Some x when String.trim x = "" -> None
            | _ -> code_gps
          in
          {x with code_gps});
    Public_data.Intitule,
    (fun state intitule x ->
       state,
       let intitule =
         match intitule with
         | Some x when String.trim x = "" -> None
         | _ -> intitule
       in
       {x with intitule});
    Public_data.Niveau,
    (fun state level x ->
       state,
       let level =
         match level with
         | Some x when String.trim x = "" -> None
         | _ -> level
       in
       {x with level});
      ]

let get_programs
    ?repository
    ?prefix
    ?file_name
    state
  =
  let event_opt = Some (Profiling.Collect_program) in
  let state =
    Remanent_state.open_event_opt
      event_opt
      state
  in
  let at_end_of_array_line
      _header state current_file current_file' output =
    match
      current_file'.code_gps
    with
    | None -> state, current_file, output
    | Some _ ->
      state, current_file, current_file'::output
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
    | None -> Remanent_state.get_programs_list_repository state
  in
  let state, list =
    Scan_csv_files.get_list
      ~keywords_of_interest ~asso_list ~keywords_list
      ~fun_default:fun_ignore
      ~at_end_of_array_line ~at_end_of_array ~at_end_of_file ~flush
      ~init_state:empty_program
      state
      ~repository ?prefix ?file_name
      []
  in
  let state =
    List.fold_left
      (fun state program ->
         match program.code_gps
         with
         | Some code_gps ->
           Remanent_state.add_program __POS__
             {Public_data.code_gps = code_gps;
              Public_data.dpt_acronym = program.dpt_acronym ;
              Public_data.level = program.level;
              Public_data.label = program.intitule;
             }
             state
         | None -> state
      )
      state list
  in
  let state =
    Remanent_state.close_event_opt
      event_opt
      state
  in
  state


type exception_id  =
  {
    dpt: string option;
    code_cours: string option;
    course_level: string option;
    student_lastname: string option;
    student_firstname: string option;
    annee_de_validation: string option
  }

let empty_exception =
  {
    dpt = None ;
    code_cours = None ;
    course_level = None ;
    student_lastname = None ;
    student_firstname = None ;
    annee_de_validation = None ;
  }


let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Departement ;
    Public_data.Code_gps;
    Public_data.Niveau;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
  ]

let keywords_of_interest =
  [
    Public_data.Departement ;
    Public_data.Code_gps;
    Public_data.Niveau;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
  ]

let asso_list =
  [
    Public_data.Departement,
    (fun state dpt x ->
       state,
       let dpt =
         match dpt with
         | Some x when String.trim x = "" -> None
         | _ -> dpt
       in
       {x with dpt});
    Public_data.Code_gps,
    (fun state code_cours x ->
       state,
       let code_cours =
         match code_cours with
         | Some x when String.trim x = "" -> None
         | _ -> code_cours
       in
       {x with code_cours});
    Public_data.Annee_Academique,
    (fun state annee_de_validation x ->
       state,
       let annee_de_validation =
         match annee_de_validation with
         | Some x when String.trim x = "" -> None
         | _ -> annee_de_validation
       in
       {x with annee_de_validation});
    Public_data.Niveau,
    (fun state course_level x ->
       state,
       let course_level =
         match course_level with
         | Some x when String.trim x = "" -> None
         | _ -> course_level
       in
       {x with course_level});
    Public_data.FirstName,
       (fun state student_firstname x ->
          state,
          let student_firstname =
            match student_firstname with
            | Some x when String.trim x = "" -> None
            | _ -> student_firstname
          in
          {x with student_firstname});
          Public_data.LastName,
             (fun state student_lastname x ->
                state,
                let student_lastname =
                  match student_lastname with
                  | Some x when String.trim x = "" -> None
                  | _ -> student_lastname
                in
                {x with student_lastname});
  ]

let get_cursus_exceptions
    ?repository
    ?prefix
    ?file_name
    state
  =
  let event_opt = Some (Profiling.Collect_cursus_exceptions) in
  let state =
    Remanent_state.open_event_opt
      event_opt
      state
  in
  let at_end_of_array_line
      _header state current_file current_file' output =
    match
      current_file'.code_cours
    with
    | None -> state, current_file, output
    | Some _ ->
      state, current_file, current_file'::output
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
    | None -> Remanent_state.get_cursus_exceptions_list_repository state
  in
  let state, list =
    Scan_csv_files.get_list
      ~keywords_of_interest ~asso_list ~keywords_list
      ~fun_default:fun_ignore
      ~at_end_of_array_line ~at_end_of_array ~at_end_of_file ~flush
      ~init_state:empty_exception
      state
      ~repository ?prefix ?file_name
      []
  in
  let state =
    List.fold_left
      (fun state exception_id ->
         match exception_id.code_cours,
               exception_id.student_firstname,
               exception_id.student_lastname,
               exception_id.annee_de_validation,
               exception_id.dpt,
               exception_id.course_level
         with
         | Some code_cours,
           Some student_firstname,
           Some student_lastname,
           Some annee_de_validation,
           Some class_dpt,
           Some class_level
           ->
           Remanent_state.add_cursus_exception __POS__
             {Public_data.codecours = code_cours;
              Public_data.student_lastname = student_lastname ;
              Public_data.student_firstname = student_firstname ;
              Public_data.class_dpt = class_dpt ;
              Public_data.annee_de_validation = annee_de_validation ;
              Public_data.class_level = class_level
                 }
             state
         | _ ->
           Remanent_state.warn
             __POS__
             "il-formed cursus exception"
             Exit
             state
      )
      state list
  in
  let state =
    Remanent_state.close_event_opt
      event_opt
      state
  in
  state
