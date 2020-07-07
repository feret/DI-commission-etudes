
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

let compute_repository =
  Remanent_state.get_monitoring_list_repository

let event_opt = Some Profiling.Collect_mentoring

let lift_pred = Scan_csv_files.lift_pred_safe
let lift = Scan_csv_files.lift_safe
let lift_opt = Scan_csv_files.lift_opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.student_lastname), "Student's family name";
    lift_pred (fun a -> a.student_firstname), "Student's first name";
    lift_pred (fun a -> a.year), "Mentoring year"
  ]

let all_fields =
  [lift
     (fun a -> a.student_lastname)
     (fun a student_lastname ->
        {a with Public_data.nom_de_l_etudiant =
                  Special_char.lowercase student_lastname})
     (Printf.sprintf "Student's family name: %s")
     __POS__ "Student's last name is missing in a mentorship description";
   lift
     (fun a -> a.student_firstname)
     (fun a student_firstname ->
        {a with Public_data.prenom_de_l_etudiant =
                  Special_char.lowercase student_firstname})
     (Printf.sprintf "Student's first name: %s")
     __POS__ "Student's first name is missing in a mentorship description";
   lift
     (fun a -> a.year)
     (fun a annee_academique ->
        {a with Public_data.annee_academique})
     (Printf.sprintf "Mentoring year: %s")
     __POS__ "Mentoring year missing in a mentorship description";
   lift_opt
     (fun a -> a.mentor_gender)
     (fun a mentor_gender ->
        {a with Public_data.genre_du_tuteur = mentor_gender})
     (fun s ->
        Printf.sprintf "Mentor Gender: %s"
          (match s with Public_data.Masculin -> "M" | Public_data.Feminin -> "F"));
   lift_opt
     (fun a -> a.mentor_firstname)
     (fun a mentor_firstname ->
        {a with Public_data.prenom_du_tuteur =
          Tools.map_opt
            Special_char.lowercase
            mentor_firstname})
     (Printf.sprintf "Mentor's first name: %s")
     ;
   lift_opt
     (fun a -> a.mentor_lastname)
     (fun a mentor_lastname ->
        {a with Public_data.nom_du_tuteur =
          Tools.map_opt
            Special_char.lowercase
            mentor_lastname})
     (Printf.sprintf "Mentor's last name: %s")
     ;
   lift_opt
     (fun a -> a.mentor_email)
     (fun a mentor_email ->
        {a with Public_data.courriel_du_tuteur =
          Tools.map_opt
            Special_char.lowercase mentor_email})
     (Printf.sprintf "Mentor's email: %s")
       ]

let get_mentoring
    ?repository
    ?prefix
    ?file_name
    state
  =
  Scan_csv_files.collect_gen
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~asso_list
    ~keywords_list
    ~init_state:empty_mentoring
    ~empty_elt:Public_data.empty_tutorat
    ~add_elt:Remanent_state.add_mentoring
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
