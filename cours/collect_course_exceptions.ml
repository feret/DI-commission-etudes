
type course_exception_id =
  {
    year: Public_data.annee option;
    code: string option;
    gender: Public_data.genre option;
    firstname: string option;
    lastname: string option;
    }


let empty_course_exception =
  {
    year = None;
    code = None;
    gender = None;
    firstname = None;
    lastname = None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Genre ;
    Public_data.Annee_Academique ;
    Public_data.Code_gps ;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Genre ;
    Public_data.Annee_Academique ;
    Public_data.Code_gps ;
  ]

let compute_repository =
  Remanent_state.Collector_course_exceptions.get_repository

let event_opt = Some Profiling.Collect_course_exceptions

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_course_exception  Public_data.empty_course_exception).Lift.safe
let lift_gender =
  (Lift.gender empty_course_exception Public_data.empty_course_exception).Lift.safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.lastname) "Student's family name";
    lift_pred (fun a -> a.firstname) "Student's first name";
    lift_pred (fun a -> a.year) "Mentoring year"
  ]

let all_fields =
  let record_name = "a course description" in
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
     ~get:(fun a -> a.Public_data.course_exception_code)
     ~set:(fun course_exception_code a ->
        {a with Public_data.course_exception_code =
                  Special_char.lowercase course_exception_code})
     ~field_name:"course code"
     ~record_name
     ~pos:__POS__;

    lift_string
     ~keyword:Public_data.LastName
     ~set_tmp:(fun state lastname x ->
         state,
         let lastname =
           match lastname with
           | Some x when String.trim x = "" -> None
           | _ -> lastname
         in
         {x with lastname})
     ~get_tmp:(fun a -> a.lastname)
     ~get:(fun a -> a.Public_data.course_exception_lastname)
     ~set:(fun course_exception_lastname a ->
        {a with Public_data.course_exception_lastname =
                  Special_char.lowercase course_exception_lastname})
     ~field_name:"teacher's family name"
     ~record_name
     ~pos:__POS__;
     lift_string
       ~keyword:Public_data.FirstName
       ~set_tmp:(fun state firstname x ->
            state,
            let firstname =
              match firstname with
              | Some x when String.trim x = "" -> None
              | _ -> firstname
            in
            {x with firstname})
        ~get_tmp:(fun a -> a.firstname)
        ~get:(fun a -> a.Public_data.course_exception_firstname)
        ~set:(fun course_exception_firstname a ->
           {a with Public_data.course_exception_firstname =
                     Special_char.lowercase course_exception_firstname})
        ~field_name:"teacher's family first name"
        ~record_name
        ~pos:__POS__;
   lift_string
     ~keyword:Public_data.Annee_Academique
     ~set_tmp:(fun state year x ->
         state, {x with year})
     ~get_tmp:(fun a -> a.year)
     ~get:(fun a -> a.Public_data.course_exception_year)
     ~set:(fun course_exception_year a ->
        {a with Public_data.course_exception_year})
     ~field_name:"mentoring year"
     ~record_name
     ~pos:__POS__ ;
   lift_gender
     ~keyword:Public_data.Genre
     ~set_tmp:(fun state gender x ->
         let state, gender =
           match
             Tools.map_opt
               (fun x -> Special_char.lowercase
                   (Special_char.correct_string_txt
                      (String.trim x)))
               gender
           with
           | Some ("m" | "masc" | "masculin") ->
             state, Some Public_data.Masculin
           | Some ("f" | "fem" | "feminin") ->
             state, Some Public_data.Feminin
           | None | Some "" -> state, None
           | Some x ->
             let msg =
               Printf.sprintf
                 "Invalid teacher's gender (%s)"
                 x
             in
             Remanent_state.warn_dft
               __POS__
               msg
               Exit
               None
               state
         in
         state, {x with gender})
     ~get_tmp:(fun a -> a.gender)
     ~get:(fun a -> a.Public_data.course_exception_genre)
     ~set:(fun course_exception_genre a ->
         {a with Public_data.course_exception_genre})
     ~field_name:"the gender of the mentor"
     ~record_name
     ~pos:__POS__;
   ]

let get_course_exceptions
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
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_course_exception
    ~empty_elt:Public_data.empty_course_exception
    ~add_elt:Remanent_state.Collector_course_exceptions.add
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
