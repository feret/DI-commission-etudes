
type dpt_id  =
  {
    acronyme: string option;
    full_name: string option;
    genitif: string option;
    fontcolor: Color.color option;
    bgcolor: Color.color option;
  }

let empty_dpt =
  {
    acronyme = None;
    full_name = None;
    genitif = None;
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
    Public_data.Genitif;
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
    Public_data.Genitif,
    (fun state genitif x ->
       state,
       let genitif =
         match genitif with
         | Some x when String.trim x = "" -> None
         | _ -> genitif
       in
       {x with genitif});
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

let event_opt = Some (Profiling.Collect_departement)
let compute_repository = Remanent_state.get_departments_list_repository

let lift_pred = Scan_csv_files.lift_pred_safe
let lift = Scan_csv_files.lift_safe
let lift_opt = Scan_csv_files.lift_opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.full_name), "the name of the department";
    lift_pred (fun a -> a.acronyme), "the acronym of the department";
    lift_pred (fun a -> a.genitif), "the genitive of the department"
  ]

let all_fields =
  [
    lift
      (fun a -> a.full_name)
      (fun a dpt_nom -> {a with Public_data.dpt_nom})
      (Printf.sprintf "Name of the department: %s")
      __POS__ "The name of the department is missing in department declaration";
    lift
        (fun a -> a.acronyme)
        (fun a dpt_acronyme -> {a with Public_data.dpt_acronyme})
        (Printf.sprintf "Acronym of the department: %s")
        __POS__ "The acronym of the department is missing in department declaration";
    lift
      (fun a -> a.genitif)
      (fun a dpt_genitif -> {a with Public_data.dpt_genitif})
      (Printf.sprintf "Genitif  of the department: %s")
      __POS__ "The genitif of the department is missing in department declaration";
    lift_opt
      (fun a -> a.bgcolor)
      (fun a dpt_bg_color -> {a with Public_data.dpt_bg_color})
      (fun x ->
         Printf.sprintf "Blackground color: %s"
           (Color.label (Color.get_background_color x)));
    lift_opt
      (fun a -> a.fontcolor)
      (fun a dpt_font_color -> {a with Public_data.dpt_font_color})
      (fun x ->
         Printf.sprintf "Font color: %s"
           (Color.label (Color.get_font_color x)))
  ]

let get_dpt
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
    ~init_state:empty_dpt
    ~empty_elt:Public_data.empty_dpt
    ~add_elt:Remanent_state.add_dpt
    ~mandatory_fields
    ~all_fields
    ?event_opt
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

let event_opt = Some (Profiling.Collect_program)
let compute_repository = Remanent_state.get_programs_list_repository

let mandatory_fields =
  [
    lift_pred (fun a -> a.code_gps),
    "Code gps of academic program is missing";
  ]

let all_fields =
  [
    lift
      (fun a -> a.code_gps)
      (fun a code_gps -> {a with Public_data.code_gps})
      (Printf.sprintf "GPS code of academic program: %s")
      __POS__ "The GPS code of an academic program is missing";
    lift_opt
      (fun a -> a.dpt_acronym)
      (fun a dpt_acronym -> {a with Public_data.dpt_acronym})
      (Printf.sprintf "Acronym of the department: %s");
    lift_opt
      (fun a -> a.intitule)
      (fun a label -> {a with Public_data.label})
      (Printf.sprintf "Program label: %s");
  ]

let get_programs
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
    ~init_state:empty_program
    ~empty_elt:Public_data.empty_program
    ~add_elt:Remanent_state.add_program
    ~mandatory_fields
    ~all_fields
    ?event_opt
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

let event_opt = Some (Profiling.Collect_cursus_exceptions)
let compute_repository = Remanent_state.get_cursus_exceptions_list_repository

let mandatory_fields =
  [
    lift_pred (fun a -> a.code_cours),
    "The code gps of a course is missing";
  ]

let all_fields =
  [
    lift
      (fun a -> a.code_cours)
      (fun a codecours -> {a with Public_data.codecours})
      (Printf.sprintf "GPS code of the course: %s")
      __POS__ "The GPS code of a course is missing in a program exception";
    lift
      (fun a -> a.student_firstname)
      (fun a student_firstname ->
         {a with Public_data.student_firstname})
      (Printf.sprintf "Student firstname: %s")
      __POS__
      "Student's first name is missing in a program exception";
      lift
        (fun a -> a.student_lastname)
        (fun a student_lastname ->
           {a with Public_data.student_lastname})
        (Printf.sprintf "Student lastname: %s")
        __POS__
        "Student's last name is missing in a program exception";
        lift
          (fun a -> a.annee_de_validation)
          (fun a annee_de_validation ->
             {a with Public_data.annee_de_validation})
          (Printf.sprintf "Validation year: %s")
          __POS__
          "The year of validation is missing in a program exception";
          lift
            (fun a -> a.dpt)
            (fun a class_dpt -> {a with Public_data.class_dpt})
            (Printf.sprintf "Department: %s")
            __POS__
            "The department is missing in a program exception";
            lift
              (fun a -> a.course_level)
              (fun a class_level -> {a with Public_data.class_level})
              (Printf.sprintf "Course level: %s")
              __POS__
              "The level of the course is missing in a program exception";
  ]

let get_cursus_exceptions
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
    ~init_state:empty_exception
    ~empty_elt:Public_data.empty_cursus_exception
    ~add_elt:Remanent_state.add_cursus_exception
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
