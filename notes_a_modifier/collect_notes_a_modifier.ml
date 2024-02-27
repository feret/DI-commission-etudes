
type notes_id  =
  {
    lastname: string option;
    firstname: string option;
    code:string option;
    note:string option;
    annee:Public_data.annee option;
    ects:float option;
  }

let empty_cours =
  {
    lastname = None;
    firstname = None;
    code = None;
    note = None;
    annee = None;
    ects = None;
  }

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Code_gps;
    Public_data.Note;
    Public_data.ECTS;
    ]

let keywords_of_interest =
  [
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Code_gps;
  ]

let event_opt =
  Some (Profiling.Collect_modified_grade)
let compute_repository =
  Remanent_state.get_modified_grades_list_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_cours Public_data.empty_note_a_modifier).Lift.safe
  let lift_string_option =
    (Lift.string empty_cours Public_data.empty_note_a_modifier).Lift.opt_safe
let lift_float_option =
  (Lift.float empty_cours Public_data.empty_note_a_modifier).Lift.opt_safe

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
]

let all_fields =
  let record_name = "a modified grade" in
  [
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(Tools.collect_string
                (fun firstname x -> { x with firstname}))
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.notetm_prenom)
      ~set:(fun notetm_prenom a ->
         {a with Public_data.notetm_prenom})
      ~field_name:"first name of the student"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(Tools.collect_string
                (fun lastname x -> { x with lastname}))
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.notetm_nom)
      ~set:(fun notetm_nom a ->
          {a with Public_data.notetm_nom})
      ~field_name:"last name of the student"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(Tools.collect_string
            (fun annee x -> { x with annee}))
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.notetm_annee)
      ~set:(fun notetm_annee a ->
             {a with Public_data.notetm_annee})
      ~field_name:"year"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.Code_gps
      ~set_tmp:(Tools.collect_string
                (fun code x -> { x with code}))
      ~get_tmp:(fun a -> a.code)
      ~get:(fun a -> a.Public_data.notetm_code)
      ~set:(fun notetm_code a ->
          {a with Public_data.notetm_code})
      ~field_name:"GPS code"
      ~pos:__POS__
      ~record_name;
    lift_string_option
      ~keyword:Public_data.Note
      ~set_tmp:(Tools.collect_string
                    (fun note x -> { x with note}))
        ~get_tmp:(fun a -> a.note)
        ~get:(fun a -> a.Public_data.notetm_note)
        ~set:(fun notetm_note a ->
            {a with Public_data.notetm_note})
        ~field_name:"NOTE"
        ~pos:__POS__
        ~record_name;
    lift_float_option
      ~keyword:Public_data.ECTS
      ~set_tmp:(collect_float "ECTS" __POS__
                  (fun ects x -> { x with ects}))
      ~get_tmp:(fun a -> a.ects)
            ~get:(fun a -> a.Public_data.notetm_ects)
            ~set:(fun notetm_ects a ->
                {a with Public_data.notetm_ects})
            ~field_name:"NOTE"
            ~pos:__POS__
            ~record_name;
  ]

let get_updated_grades
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
    ~empty_elt:Public_data.empty_note_a_modifier
    ~add_elt:Remanent_state.add_note_a_modifier
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
