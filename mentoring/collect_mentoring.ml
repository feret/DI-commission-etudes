
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

let compute_repository =
  Remanent_state.get_monitoring_list_repository

let event_opt = Some Profiling.Collect_mentoring

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_mentoring Public_data.empty_tutorat).Lift.safe
let lift_string_opt =
  (Lift.string empty_mentoring Public_data.empty_tutorat).Lift.opt_safe
let lift_gender_opt =
  (Lift.gender empty_mentoring Public_data.empty_tutorat).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.student_lastname) "Student's family name";
    lift_pred (fun a -> a.student_firstname) "Student's first name";
    lift_pred (fun a -> a.year) "Mentoring year"
  ]

let all_fields =
  let record_name = "a mentorship description" in
  [lift_string
     ~keyword:Public_data.LastName
     ~set_tmp:(fun state student_lastname x ->
         state,
         let student_lastname =
           match student_lastname with
           | Some x when String.trim x = "" -> None
           | _ -> student_lastname
         in
         {x with student_lastname})
     ~get_tmp:(fun a -> a.student_lastname)
     ~get:(fun a -> a.Public_data.nom_de_l_etudiant)
     ~set:(fun student_lastname a ->
        {a with Public_data.nom_de_l_etudiant =
                  Special_char.lowercase student_lastname})
     ~field_name:"student's family name"
     ~record_name
     ~pos:__POS__;
   lift_string
     ~keyword:Public_data.FirstName
     ~set_tmp:(fun state student_firstname x ->
         state,
         let student_firstname =
           match student_firstname with
           | Some x when String.trim x = "" -> None
           | _ -> student_firstname
         in
         {x with student_firstname})
     ~get_tmp:(fun a -> a.student_firstname)
     ~get:(fun a -> a.Public_data.prenom_de_l_etudiant)
     ~set:(fun student_firstname a ->
        {a with Public_data.prenom_de_l_etudiant =
                  Special_char.lowercase student_firstname})
     ~field_name:"student's first name"
     ~record_name
     ~pos:__POS__ ;
   lift_string
     ~keyword:Public_data.Annee_Academique
     ~set_tmp:(fun state year x ->
         state, {x with year})
     ~get_tmp:(fun a -> a.year)
     ~get:(fun a -> a.Public_data.annee_academique)
     ~set:(fun annee_academique a ->
        {a with Public_data.annee_academique})
     ~field_name:"mentoring year"
     ~record_name
     ~pos:__POS__ ;
   lift_gender_opt
     ~keyword:Public_data.Genre_du_tuteur
     ~set_tmp:(fun state mentor_gender x ->
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
           | None | Some "" -> state, None
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
         state, {x with mentor_gender})
     ~get_tmp:(fun a -> a.mentor_gender)
     ~get:(fun a -> a.Public_data.genre_du_tuteur)
     ~set:(fun mentor_gender a ->
        {a with Public_data.genre_du_tuteur = mentor_gender})
     ~field_name:"the gender of the mentor"
     ~record_name
     ~pos:__POS__;
   lift_string_opt
     ~keyword:Public_data.Prenom_du_tuteur
     ~set_tmp:(fun state mentor_firstname x ->
         state,
         let mentor_firstname =
           match mentor_firstname with
           | Some x when String.trim x = "" -> None
           | _ -> mentor_firstname
         in
         {x with mentor_firstname})
     ~get_tmp:(fun a -> a.mentor_firstname)
     ~get:(fun a -> a.Public_data.prenom_du_tuteur)
     ~set:(fun mentor_firstname a ->
        {a with Public_data.prenom_du_tuteur =
          Tools.map_opt
            Special_char.lowercase
            mentor_firstname})
     ~field_name:"mentor's first name"
     ~record_name
     ~pos:__POS__;
   lift_string_opt
     ~keyword:Public_data.Nom_du_tuteur
     ~set_tmp:(fun state mentor_lastname x ->
         state,
         let mentor_lastname =
           match mentor_lastname with
           | Some x when String.trim x = "" -> None
           | _ -> mentor_lastname
         in
         {x with mentor_lastname})
     ~get_tmp:(fun a -> a.mentor_lastname)
     ~get:(fun a -> a.Public_data.nom_du_tuteur)
     ~set:(fun mentor_lastname a ->
        {a with Public_data.nom_du_tuteur =
          Tools.map_opt
            Special_char.lowercase
            mentor_lastname})
     ~field_name:"mentor's last name"
     ~record_name
     ~pos:__POS__ ;
   lift_string_opt
     ~keyword:Public_data.Courriel_du_tuteur
     ~set_tmp:(fun state mentor_email x ->
         state,
         let mentor_email =
           match mentor_email with
           | Some x when String.trim x = "" -> None
           | _ -> mentor_email
         in
         {x with mentor_email})
     ~get_tmp:(fun a -> a.mentor_email)
     ~get:(fun a -> a.Public_data.courriel_du_tuteur)
     ~set:(fun mentor_email a ->
        {a with Public_data.courriel_du_tuteur =
          Tools.map_opt
            Special_char.lowercase mentor_email})
     ~field_name:"mentor's email"
     ~record_name
     ~pos:__POS__
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
    ~keywords_list
    ~init_state:empty_mentoring
    ~empty_elt:Public_data.empty_tutorat
    ~add_elt:Remanent_state.add_mentoring
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
