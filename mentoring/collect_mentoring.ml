
type mentoring_id =
  {
    student_lastname: string option;
    student_firstname: string option;
    year: string option;
    mentor_lastname: string option;
    mentor_firstname: string option;
    mentor_gender: Public_data.genre option;
    mentor_email: string option
  }


let empty_mentoring =
  {
    student_lastname = None ;
    student_firstname = None ;
    year = None ;
    mentor_lastname = None ;
    mentor_firstname = None ;
    mentor_gender = None ;
    mentor_email = None ;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Annee_Academique ;
    Public_data.Nom_du_tuteur ;
    Public_data.Prenom_du_tuteur ;
    Public_data.Genre_du_tuteur ;
    Public_data.Courriel_du_tuteur;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Annee_Academique ;
  ]
let asso_list =
  [
    Public_data.LastName,
    (fun state student_lastname x ->
       state,
       let student_lastname =
         match student_lastname with
         | Some x when String.trim x = "" -> None
         | _ -> student_lastname
       in
       {x with student_lastname});
    Public_data.FirstName,
    (fun state student_firstname x ->
       state,
       let student_firstname =
         match student_firstname with
         | Some x when String.trim x = "" -> None
         | _ -> student_firstname
       in
       {x with student_firstname});
    Public_data.Annee_Academique,
    (fun state year x ->
       state, {x with year});
    Public_data.Nom_du_tuteur ,
    (fun state mentor_lastname x ->
       state,
       let mentor_lastname =
         match mentor_lastname with
         | Some x when String.trim x = "" -> None
         | _ -> mentor_lastname
       in
       {x with mentor_lastname});
    Public_data.Prenom_du_tuteur,
    (fun state mentor_firstname x ->
       state,
       let mentor_firstname =
            match mentor_firstname with
            | Some x when String.trim x = "" -> None
            | _ -> mentor_firstname
          in
          {x with mentor_firstname});
    Public_data.Courriel_du_tuteur,
    (fun state mentor_email x ->
       state,
       let mentor_email =
         match mentor_email with
         | Some x when String.trim x = "" -> None
         | _ -> mentor_email
       in
       {x with mentor_email});
    Public_data.Genre_du_tuteur,
    (fun state mentor_gender x ->
          let state, mentor_gender =
            match
              Tools.map_opt
                (fun x -> Special_char.lowercase
                    (Special_char.correct_string_txt
                       (String.trim x)))
              mentor_gender
            with
            | Some ("m" | "masc" | "masculin") ->
              state, Some Public_data.Masculin
            | Some ("f" | "fem" | "feminin") ->
              state, Some Public_data.Feminin
            | None -> state, None
            | Some x ->
              let msg =
                Printf.sprintf
                  "Invalid mentor's gender (%s)"
                  x
              in
              Remanent_state.warn_dft
                __POS__
                msg
                Exit
                None
                state
          in
          state, {x with mentor_gender});
  ]

let get_mentoring
    ?repository
    ?prefix
    ?file_name
    state
  =
  let event_opt = Some (Profiling.Collect_mentoring) in
  let state =
    Remanent_state.open_event_opt
      event_opt
      state
  in
  let at_end_of_array_line
      _header state current_file current_file' output =
    match current_file'.student_firstname,current_file'.student_lastname,current_file'.year with
    | None,None,None -> state, current_file, output
    | Some _, Some _, Some _ ->
      state, current_file, current_file'::output
    | Some x, None, Some y ->
      let msg = Format.sprintf "Last name is missing for %s's mentoring %s" x y in
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
        Format.sprintf "Year is missing for %s %s's mentoring"
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
    | None, Some x, Some y ->
      let msg = Format.sprintf "First name is missing for %s's mentoring %s" x y in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | Some x, None, None ->
      let msg = Format.sprintf "Last name and year is missing for %s's mentoring" x in
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
        Format.sprintf "First name and year is missing fos %s's mentoring" y
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
      let msg = Format.sprintf "Name and first names are missing for a mentoring %s" y in
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
    | None -> Remanent_state.get_monitoring_list_repository state
  in
  let state, list =
    Scan_csv_files.get_list
      ~keywords_of_interest ~asso_list ~keywords_list
      ~fun_default:fun_ignore
      ~at_end_of_array_line ~at_end_of_array ~at_end_of_file ~flush
      ~init_state:empty_mentoring
      state
      ~repository ?prefix ?file_name
      []
  in
  let state =
    List.fold_left
      (fun state mentoring ->
         match mentoring.student_firstname, mentoring.student_lastname, mentoring.year
         with
         | Some firstname, Some lastname, Some year ->
           Remanent_state.add_mentoring __POS__
             {Public_data.nom_de_l_etudiant =
                String.lowercase_ascii lastname ;
              Public_data.prenom_de_l_etudiant=
                String.lowercase_ascii firstname;
              Public_data.annee_academique=
                year;
              Public_data.nom_du_tuteur =
                Tools.map_opt
                  String.lowercase_ascii mentoring.mentor_lastname;
              Public_data.prenom_du_tuteur =
                Tools.map_opt
                  String.lowercase_ascii mentoring.mentor_firstname;
              Public_data.genre_du_tuteur =
                mentoring.mentor_gender; 
              Public_data.courriel_du_tuteur =
                Tools.map_opt
                  String.lowercase_ascii mentoring.mentor_email
             }
             state
         | None, None, None -> state
         | Some x, None, Some y ->
           let msg = Format.sprintf "Last name is missing for %s's mentoring %s" x y in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
         | Some x, Some y, None  ->
             let msg =
               Format.sprintf
                 "Year is missing fos %s %s's mentoring" x y
             in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, Some x, Some y ->
             let msg = Format.sprintf "First name is missing for %s's mentoring %s" x y in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | Some x, None, None ->
             let msg = Format.sprintf "Last name and year are missing for %s's mentoring" x in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, Some y, None  ->
             let msg =
               Format.sprintf "First name and year are missing for %s 's mentoring" y
             in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, None, Some y ->
             let msg = Format.sprintf "First and last names are missing for a mentoring %s" y in
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
