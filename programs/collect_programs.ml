
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

let event_opt = Some (Profiling.Collect_departement)
let compute_repository = Remanent_state.get_departments_list_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_dpt Public_data.empty_dpt).Lift.safe
let lift_color_opt =
  (Lift.color empty_dpt Public_data.empty_dpt).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.full_name) "the name of the department";
    lift_pred (fun a -> a.acronyme) "the acronym of the department";
    lift_pred (fun a -> a.genitif) "the genitive of the department"
  ]

let all_fields =
  let record_name = "departement declaration" in
  [
    lift_string
      ~keyword:Public_data.FullName
      ~set_tmp:(fun state full_name x ->
          state,
          let full_name =
            match full_name with
            | Some x when String.trim x = "" -> None
            | _ -> full_name
          in
          {x with full_name})
      ~get_tmp:(fun a -> a.full_name)
      ~get:(fun a -> a.Public_data.dpt_nom)
      ~set:(fun dpt_nom a -> {a with Public_data.dpt_nom})
      ~field_name:"name of the department"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Acronyme
      ~set_tmp:(fun state acronyme x ->
          state,
          let acronyme =
            match acronyme with
            | Some x when String.trim x = "" -> None
            | _ -> acronyme
          in
          {x with acronyme})
      ~get_tmp:(fun a -> a.acronyme)
      ~get:(fun a -> a.Public_data.dpt_acronyme)
      ~set:(fun dpt_acronyme a -> {a with Public_data.dpt_acronyme})
      ~field_name:"acronym of the department"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Genitif
      ~set_tmp:(fun state genitif x ->
          state,
          let genitif =
            match genitif with
            | Some x when String.trim x = "" -> None
            | _ -> genitif
          in
          {x with genitif})
      ~get_tmp:(fun a -> a.genitif)
      ~get:(fun a -> a.Public_data.dpt_genitif)
      ~set:(fun dpt_genitif a -> {a with Public_data.dpt_genitif})
      ~field_name:"genitif of the department"
      ~record_name
      ~pos:__POS__ ;
    lift_color_opt
      ~keyword:Public_data.Couleur_du_fond
      ~set_tmp:(fun state bgcolor x ->
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
       state, {x with bgcolor})

      ~get_tmp:(fun a -> a.bgcolor)
      ~get:(fun a -> a.Public_data.dpt_bg_color)
      ~set:(fun dpt_bg_color a -> {a with Public_data.dpt_bg_color})
      ~field_name:"background color"
      ~record_name
      ~pos:__POS__ ;
    lift_color_opt
      ~keyword:Public_data.Couleur_du_texte
      ~set_tmp:(fun state fontcolor x ->
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
          state, {x with fontcolor})
      ~get_tmp:(fun a -> a.fontcolor)
      ~get:(fun a -> a.Public_data.dpt_font_color)
      ~set:(fun dpt_font_color a -> {a with Public_data.dpt_font_color})
      ~field_name:"font color"
      ~record_name
      ~pos:__POS__ ;
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

let lift_string =
  (Lift.string empty_program Public_data.empty_program).Lift.safe
let lift_string_opt =
  (Lift.string empty_program Public_data.empty_program).Lift.opt_safe

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

let event_opt = Some (Profiling.Collect_program)
let compute_repository = Remanent_state.get_programs_list_repository

let mandatory_fields =
  [
    lift_pred (fun a -> a.code_gps)
    "Code gps of academic program is missing";
  ]

let all_fields =
  let record_name = "academic program" in
  [
    lift_string
      ~keyword:Public_data.Code_gps
      ~set_tmp:(fun state code_gps x ->
          state,
          let code_gps =
            match code_gps with
            | Some x when String.trim x = "" -> None
            | _ -> code_gps
          in
          {x with code_gps})
      ~get_tmp:(fun a -> a.code_gps)
      ~get:(fun a -> a.Public_data.code_gps)
      ~set:(fun code_gps a -> {a with Public_data.code_gps})
      ~field_name:"GPS code"
      ~record_name
      ~pos:__POS__;
  lift_string_opt
    ~keyword:Public_data.Departement
    ~set_tmp:(fun state dpt_acronym x ->
        state,
        let dpt_acronym =
          match dpt_acronym with
          | Some x when String.trim x = "" -> None
          | _ -> dpt_acronym
        in
        {x with dpt_acronym})
    ~get_tmp:(fun a -> a.dpt_acronym)
    ~get:(fun a -> a.Public_data.dpt_acronym)
    ~set:(fun dpt_acronym a -> {a with Public_data.dpt_acronym})
    ~field_name:"acronym of the department"
    ~record_name
    ~pos:__POS__;
  lift_string_opt
    ~keyword:Public_data.Intitule
    ~set_tmp:(fun state intitule x ->
        state,
        let intitule =
          match intitule with
          | Some x when String.trim x = "" -> None
          | _ -> intitule
        in
        {x with intitule})
    ~get_tmp:(fun a -> a.intitule)
    ~get:(fun a -> a.Public_data.label)
    ~set:(fun label a -> {a with Public_data.label})
    ~field_name:"label of the program"
    ~record_name
    ~pos:__POS__;

    lift_string_opt
      ~keyword:Public_data.Niveau
      ~set_tmp:(fun state level x ->
          state,
          let level =
            match level with
            | Some x when String.trim x = "" -> None
            | _ -> level
          in
          {x with level})
      ~get_tmp:(fun a -> a.level)
      ~get:(fun a -> a.Public_data.level)
      ~set:(fun level a -> {a with Public_data.level})      ~field_name:"niveau"
      ~record_name
      ~pos:__POS__
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

let event_opt = Some (Profiling.Collect_cursus_exceptions)
let compute_repository =
  Remanent_state.get_cursus_exceptions_list_repository

let lift_string =
  (Lift.string empty_exception Public_data.empty_cursus_exception).Lift.safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.code_cours) "The code gps of a course is missing";
  ]

let all_fields =
  let record_name = "a program exception" in
  [
    lift_string
      ~keyword:Public_data.Code_gps
      ~set_tmp:(fun state code_cours x ->
          state,
          let code_cours =
            match code_cours with
            | Some x when String.trim x = "" -> None
            | _ -> code_cours
          in
       {x with code_cours})
      ~get_tmp:(fun a -> a.code_cours)
      ~get:(fun a -> a.Public_data.codecours)
      ~set:(fun codecours a -> {a with Public_data.codecours})
      ~field_name:"GPS code"
      ~record_name
      ~pos:__POS__ ;
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
      ~get:(fun a -> a.Public_data.student_firstname)
      ~set:(fun student_firstname a ->
         {a with Public_data.student_firstname})
      ~field_name:"the first name of the student"
      ~record_name
      ~pos:__POS__ ;
    lift_string
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
      ~get:(fun a -> a.Public_data.student_lastname)
      ~set:(fun student_lastname a ->
          {a with Public_data.student_lastname})
      ~field_name:"the last name of the student"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(fun state annee_de_validation x ->
          state,
          let annee_de_validation =
            match annee_de_validation with
            | Some x when String.trim x = "" -> None
            | _ -> annee_de_validation
          in
          {x with annee_de_validation})
      ~get_tmp:(fun a -> a.annee_de_validation)
      ~get:(fun a -> a.Public_data.annee_de_validation)
      ~set:(fun annee_de_validation a ->
             {a with Public_data.annee_de_validation})
      ~field_name:"validation year"
      ~record_name
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Departement
      ~set_tmp:(fun state dpt x ->
          state,
          let dpt =
            match dpt with
            | Some x when String.trim x = "" -> None
            | _ -> dpt
          in
          {x with dpt})
      ~get_tmp:(fun a -> a.dpt)
      ~get:(fun a -> a.Public_data.class_dpt)
      ~set:(fun class_dpt a-> {a with Public_data.class_dpt})
      ~field_name:"departement"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Niveau
      ~set_tmp:(fun state course_level x ->
          state,
          let course_level =
            match course_level with
            | Some x when String.trim x = "" -> None
            | _ -> course_level
          in
          {x with course_level})
      ~get_tmp:(fun a -> a.course_level)
      ~get:(fun a -> a.Public_data.class_level)
      ~set:(fun class_level a -> {a with Public_data.class_level})
      ~field_name:"course level"
      ~record_name
      ~pos:__POS__ ;
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
    ~keywords_list
    ~init_state:empty_exception
    ~empty_elt:Public_data.empty_cursus_exception
    ~add_elt:Remanent_state.add_cursus_exception
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
