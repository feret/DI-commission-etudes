
type course_name_translation_id =
  {
    year: Public_data.annee option;
    code: string option;
    name: string option;
    name_en: string option;
  }


let empty_course_name_translation =
  {
    year = None;
    code = None;
    name = None;
    name_en = None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Annee_Academique ;
    Public_data.Code_gps ;
    Public_data.Name ;
    Public_data.Name_en ;
  ]

let keywords_of_interest =
  [
    Public_data.Annee_Academique ;
    Public_data.Code_gps ;
  ]

let compute_repository =
  Remanent_state.get_course_name_translation_list_repository

let event_opt = Some Profiling.Collect_course_name_translations

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_course_name_translation  Public_data.empty_course_name_translation).Lift.safe
let lift_string_opt =
  (Lift.string empty_course_name_translation Public_data.empty_course_name_translation).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.code) "GPS code";
    lift_pred (fun a -> a.year) "Academic year";
  ]

let all_fields =
  let record_name = "a course name translation" in
  [lift_string
     ~keyword:Public_data.Code_gps
     ~set_tmp:(fun state code x ->
         state,
         let code =
           match code with
           | Some x when String.trim x = "" -> None
           | _ -> code
         in
         {x with code})
     ~get_tmp:(fun a -> a.code)
     ~get:(fun a -> a.Public_data.code)
     ~set:(fun code a ->
        {a with Public_data.code =
                  Special_char.lowercase code})
     ~field_name:"course code"
     ~record_name
     ~pos:__POS__;

   lift_string
     ~keyword:Public_data.Annee_Academique
     ~set_tmp:(fun state year x ->
         state, {x with year})
     ~get_tmp:(fun a -> a.year)
     ~get:(fun a -> a.Public_data.year)
     ~set:(fun year a ->
        {a with Public_data.year})
     ~field_name:"course year"
     ~record_name
     ~pos:__POS__ ;
   lift_string_opt
     ~keyword:Public_data.Name
     ~set_tmp:(fun state name x -> state, {x with name})
     ~get_tmp:(fun a -> a.name)
     ~get:(fun a -> a.Public_data.name)
     ~set:(fun name a ->
         {a with Public_data.name})
     ~field_name:"French name of the class"
     ~record_name
     ~pos:__POS__;
lift_string_opt
  ~keyword:Public_data.Name_en
  ~set_tmp:(fun state name_en x -> state,{x with name_en})
~get_tmp:(fun a -> a.name_en)
  ~get:(fun a -> a.Public_data.name_en)
  ~set:(fun name_en a ->
      {a with Public_data.name_en})
  ~field_name:"English name of the class"
  ~record_name
  ~pos:__POS__;

]

let get_course_name_translations
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
    ~init_state:empty_course_name_translation
    ~empty_elt:Public_data.empty_course_name_translation
    ~add_elt:Remanent_state.add_course_name_translation
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
