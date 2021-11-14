type admission_id  =
  {
    firstname: string option;
    lastname: string option;
    admission: string option;
    admission_en: string option;
    annee: string option;
  }

let empty_admission =
  {
    firstname=None;
    lastname=None;
    annee=None;
    admission=None;
    admission_en=None;
  }

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Decision;
    Public_data.Decision_en;
    Public_data.Annee_Academique;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Annee_Academique
  ]

let event_opt = Some (Profiling.Collect_admissions)
let compute_repository =
  Remanent_state.get_admissions_list_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_admission Public_data.empty_admission).Lift.safe
let lift_string_opt =
  (Lift.string empty_admission Public_data.empty_admission).Lift.opt_safe
let mandatory_fields =
  [
    lift_pred (fun a -> a.annee) "academic year";
    lift_pred (fun a -> a.firstname) "the first name of the student";
    lift_pred (fun a -> a.lastname) "the last name of the student";
    lift_pred (fun a -> a.admission) "the decision";
  ]

let all_fields =
  let record_name = "M2 admission" in
  [
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(Tools.collect_string (fun firstname x -> {x with firstname}))
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.admission_firstname)
      ~set:(fun admission_firstname a ->
         {a with Public_data.admission_firstname})
      ~record_name
      ~field_name:"first name of the student"
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(Tools.collect_string (fun lastname x -> {x with lastname}))
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.admission_lastname)
      ~set:(fun admission_lastname a ->
          {a with Public_data.admission_lastname})
      ~record_name
      ~field_name:"last name of the student"
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(Tools.collect_string (fun annee x -> {x with annee}))
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.admission_annee)
      ~set:(fun admission_annee a ->
          {a with Public_data.admission_annee})
      ~record_name
      ~field_name:"year of the program"
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Decision
      ~set_tmp:(Tools.collect_string
                  (fun admission x -> {x with admission}))
      ~get_tmp:(fun a -> a.admission)
      ~get:(fun a -> a.Public_data.admission_decision)
      ~set:(fun admission_decision a ->
          {a with Public_data.admission_decision})
      ~record_name
      ~field_name:"M2 admission"
      ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Decision_en
      ~set_tmp:(Tools.collect_string
                  (fun admission_en x -> {x with admission_en}))
      ~get_tmp:(fun a -> a.admission_en)
      ~get:(fun a -> a.Public_data.admission_decision_en)
      ~set:(fun admission_decision_en a ->
          {a with Public_data.admission_decision_en})
      ~record_name
      ~field_name:"M2 admission (english)"
      ~pos:__POS__;
  ]

let get_admissions
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
    ~init_state:empty_admission
    ~empty_elt:Public_data.empty_admission
    ~add_elt:Remanent_state.add_admission
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
