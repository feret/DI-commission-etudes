
type course_entries_id =
  {
    gps_entry: string option;
    french_entry: string option;
    english_entry: string option;
  }


let empty_course_entry =
  {
    gps_entry = None;
    french_entry = None;
    english_entry = None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Entree_GPS ;
    Public_data.Libelle ;
    Public_data.Label ;
  ]

let keywords_of_interest =
  [
    Public_data.Entree_GPS ;
  ]

let compute_repository = Remanent_state.get_course_entry_list_repository
let event_opt = Some Profiling.Collect_course_entries

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_course_entry  Public_data.empty_course_entry).Lift.safe
let lift_string_opt =
  (Lift.string empty_course_entry Public_data.empty_course_entry).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> Some a.gps_entry) "GPS entry";
  ]

let all_fields =
  let record_name = "a course name translation" in
  [lift_string
     ~keyword:Public_data.Entree_GPS
     ~set_tmp:(fun state gps_entry x -> state, {x with gps_entry})
     ~get_tmp:(fun a -> a.gps_entry)
     ~get:(fun a -> a.Public_data.gps_entry)
     ~set:(fun gps_entry a ->
         {a with Public_data.gps_entry})
     ~field_name:"gps entry"
     ~record_name
     ~pos:__POS__;

   lift_string_opt
     ~keyword:Public_data.Libelle
     ~set_tmp:(fun state french_entry x ->
         state, {x with french_entry})
     ~get_tmp:(fun a -> a.french_entry)
     ~get:(fun a -> a.Public_data.french_entry)
     ~set:(fun french_entry a ->
        {a with Public_data.french_entry})
     ~field_name:"nom français"
     ~record_name
     ~pos:__POS__ ;
   lift_string_opt
     ~keyword:Public_data.Label
     ~set_tmp:(fun state english_entry x -> state, {x with english_entry})
     ~get_tmp:(fun a -> a.english_entry)
     ~get:(fun a -> a.Public_data.english_entry)
     ~set:(fun english_entry a ->
         {a with Public_data.english_entry})
     ~field_name:"English name of the class"
     ~record_name
     ~pos:__POS__;
]

let get_course_entries
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
    ~init_state:empty_course_entry
    ~empty_elt:Public_data.empty_course_entry
    ~add_elt:Remanent_state.Translate_courses.Collector.add
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state

let unify_course_entry =
  Scan_gen_files.unify_gen ~all_fields
