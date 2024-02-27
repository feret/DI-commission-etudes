
type cours_id  =
  {
    lastname: string option;
    firstname: string option;
    code:string option;
    libelle:string option;
    dpt:string option;
    level:string option;
    note:float option;
    ects:float option;
    annee:Public_data.annee option;
  }

let empty_cours =
  {
    lastname = None;
    firstname = None;
    code = None;
    libelle = None;
    dpt = None;
    level = None;
    note = None;
    ects = None;
    annee = None;
  }

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Code_gps;
    Public_data.Libelle;
    Public_data.Grade;
    Public_data.ECTS;
    Public_data.Departement;
    Public_data.Niveau
  ]

let keywords_of_interest =
  [
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.ECTS
  ]

let event_opt =
  Some (Profiling.Collect_additional_courses)
let compute_repository =
  Remanent_state.get_additional_courses_list_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_cours Public_data.empty_cours_a_ajouter).Lift.safe
let lift_string_opt =
    (Lift.string empty_cours Public_data.empty_cours_a_ajouter).Lift.opt_safe
let lift_float_opt =
  (Lift.float empty_cours Public_data.empty_cours_a_ajouter).Lift.opt_safe
let lift_float =
  (Lift.float empty_cours Public_data.empty_cours_a_ajouter).Lift.safe

let collect_float suffix pos state =
  Tools.collect_float
    (fun msg state ->
       let msg = msg^suffix in
       Remanent_state.warn
         pos
         msg
         Exit
         state
    )
    state

let mandatory_fields =
  [
    lift_pred (fun a -> a.firstname) "The first name of the student";
    lift_pred (fun a -> a.lastname) "The last name of the student";
    lift_pred (fun a -> a.libelle) "The name of the class";
]

let all_fields =
  let record_name = "a not-in-gps course entry" in
  [
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(Tools.collect_string
                (fun firstname x -> { x with firstname}))
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.coursaj_prenom)
      ~set:(fun coursaj_prenom a ->
         {a with Public_data.coursaj_prenom})
      ~field_name:"first name of the student"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(Tools.collect_string
                (fun lastname x -> { x with lastname}))
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.coursaj_nom)
      ~set:(fun coursaj_nom a ->
          {a with Public_data.coursaj_nom})
      ~field_name:"last name of the student"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(Tools.collect_string
            (fun annee x -> { x with annee}))
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.coursaj_annee)
      ~set:(fun coursaj_annee a ->
             {a with Public_data.coursaj_annee})
      ~field_name:"year"
      ~pos:__POS__
      ~record_name;
    lift_string_opt
      ~keyword:Public_data.Code_gps
      ~set_tmp:(Tools.collect_string
                (fun code x -> { x with code}))
      ~get_tmp:(fun a -> a.code)
      ~get:(fun a -> a.Public_data.coursaj_code)
      ~set:(fun coursaj_code a ->
          {a with Public_data.coursaj_code})
      ~field_name:"GPS code"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.Libelle
      ~set_tmp:(Tools.collect_string
                  (fun libelle x -> { x with libelle}))
      ~get_tmp:(fun a -> a.libelle)
      ~get:(fun a -> a.Public_data.coursaj_libelle)
      ~set:(fun coursaj_libelle a ->
          {a with Public_data.coursaj_libelle})
      ~field_name:"COURSE NAME"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.Niveau
      ~set_tmp:(Tools.collect_string (fun level x -> { x with level}))
        ~get_tmp:(fun a -> a.level)
        ~get:(fun a -> a.Public_data.coursaj_level)
        ~set:(fun coursaj_level a ->
            {a with Public_data.coursaj_level})
        ~field_name:"COURSE LEVEL"
        ~pos:__POS__
        ~record_name;
    lift_string_opt
      ~keyword:Public_data.Departement
      ~set_tmp:(Tools.collect_string (fun dpt x -> { x with dpt}))
            ~get_tmp:(fun a -> a.dpt)
            ~get:(fun a -> a.Public_data.coursaj_dpt)
            ~set:(fun coursaj_dpt a ->
                {a with Public_data.coursaj_dpt})
            ~field_name:"COURSE DPT"
            ~pos:__POS__
            ~record_name;
    lift_float
      ~keyword:Public_data.ECTS
      ~set_tmp:(collect_float "ects" __POS__
                  (fun ects x -> { x with ects}))
      ~get_tmp:(fun a -> a.ects)
      ~get:(fun a -> a.Public_data.coursaj_ects)
      ~set:(fun coursaj_ects a ->
          {a with Public_data.coursaj_ects})
      ~field_name:"ECTS"
      ~pos:__POS__
      ~record_name;
    lift_float_opt
      ~keyword:Public_data.Grade
      ~set_tmp:(collect_float "Note" __POS__
                    (fun note x -> { x with note}))
        ~get_tmp:(fun a -> a.note)
        ~get:(fun a -> a.Public_data.coursaj_note)
        ~set:(fun coursaj_note a ->
            {a with Public_data.coursaj_note})
        ~field_name:"NOTE"
        ~pos:__POS__
        ~record_name;
    (*   Public_data.ECTS;
      Public_data.Departement;
        *)

  ]

let get_additional_courses
    ?repository
    ?prefix
    ?file_name
    state
  =
  Scan_csv_files.collect_gen
    ~strict:true 
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:Tools.fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_cours
    ~empty_elt:Public_data.empty_cours_a_ajouter
    ~add_elt:Remanent_state.add_additional_course
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
