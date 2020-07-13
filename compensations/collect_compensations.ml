
type compensation_id  =
  {
    firstname: string option;
    lastname: string option;
    annee: string option;
    codecours: string option;
  }

let empty_compensation =
  {
    firstname = None;
    lastname = None;
    annee = None;
    codecours = None;
  }

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Code_gps;
  ]

let keywords_of_interest =
  [
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Code_gps;
  ]

let event_opt = Some (Profiling.Collect_compensations)
let compute_repository =
  Remanent_state.get_compensations_list_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_compensation Public_data.empty_compensation).Lift.safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.firstname) "The first name of the student";
    lift_pred (fun a -> a.lastname) "The last name of the student";
]

let all_fields =
  let record_name = "a grade compensation" in
  [
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(Tools.collect_string
                (fun firstname x -> { x with firstname}))
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.comp_firstname)
      ~set:(fun comp_firstname a ->
         {a with Public_data.comp_firstname})
      ~field_name:"first name of the student"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(Tools.collect_string
                (fun lastname x -> { x with lastname}))
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.comp_lastname)
      ~set:(fun comp_lastname a ->
          {a with Public_data.comp_lastname})
      ~field_name:"last name of the student"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(Tools.collect_string
            (fun annee x -> { x with annee}))
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.comp_annee)
      ~set:(fun comp_annee a ->
             {a with Public_data.comp_annee})
      ~field_name:"year"
      ~pos:__POS__
      ~record_name;
    lift_string
      ~keyword:Public_data.Code_gps
      ~set_tmp:(Tools.collect_string
                (fun codecours x -> { x with codecours}))
      ~get_tmp:(fun a -> a.codecours)
      ~get:(fun a -> a.Public_data.comp_codecours)
      ~set:(fun comp_codecours a ->
          {a with Public_data.comp_codecours})
      ~field_name:"GPS code"
      ~pos:__POS__
      ~record_name;
  ]

let get_compensations
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
    ~fun_default:Tools.fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_compensation
    ~empty_elt:Public_data.empty_compensation
    ~add_elt:Remanent_state.add_compensation
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
