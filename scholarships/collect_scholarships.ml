
type scholarship_id =
  {
    lastname: string option;
    firstname: string option;
    promotion: string option;
    organism: string option;
    debut: string option;
    fin: string option
  }


let empty_scholarship =
  {
    lastname = None ;
    firstname = None ;
    promotion = None ;
    organism = None ;
    debut = None ;
    fin = None ;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Promo ;
    Public_data.Organisme_de_Financement ;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
  ]

let event_opt = Some Profiling.Collect_scholarships

let lift_string =
  (Lift.string empty_scholarship Public_data.empty_scholarship).Lift.safe
let lift_pred = Lift.pred_safe
let lift_string_opt =
  (Lift.string empty_scholarship Public_data.empty_scholarship).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.lastname) "Student's family name";
    lift_pred (fun a -> a.firstname) "Student's first name";
    lift_pred (fun a -> a.promotion) "Student's promotion"
  ]

let all_fields =
  let record_name = "a scholarship description" in
  [lift_string
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
     ~get:(fun a -> a.Public_data.holder_lastname)
     ~set:(fun holder_lastname a ->
         {a with Public_data.holder_lastname})
     ~field_name:"the family name of the student"
     ~record_name
     ~pos:__POS__ ;
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
     ~get:(fun a -> a.Public_data.holder_firstname)
     ~set:(fun holder_firstname a -> {a with Public_data.holder_firstname = holder_firstname})
     ~field_name:"the first name of the student"
     ~record_name
     ~pos:__POS__;
   lift_string_opt
     ~keyword:Public_data.Promo
     ~set_tmp:(fun state promotion x -> state, {x with promotion})
     ~get_tmp:(fun a -> a.promotion)
     ~get:(fun a -> a.Public_data.holder_promotion)
     ~set:(fun holder_promotion a -> {a with Public_data.holder_promotion})
     ~field_name:"the promotion year of the student"
     ~record_name
     ~pos:__POS__;
   lift_string
     ~keyword:Public_data.Organisme_de_Financement
     ~set_tmp:(fun state organism x -> state, {x with organism})
     ~get_tmp:(fun a -> a.organism)
     ~get:(fun a -> a.Public_data.organism)
     ~set:(fun organism a -> {a with Public_data.organism})
     ~field_name:"Funding organism"
     ~record_name
     ~pos:__POS__;
   lift_string_opt
     ~keyword:Public_data.Annee_Debut
     ~set_tmp:(fun state debut x -> state, {x with debut})
     ~get_tmp:(fun a -> a.debut)
     ~get:(fun a -> a.Public_data.funding_begin)
     ~set:(fun funding_begin a -> {a with Public_data.funding_begin})
     ~field_name:"the starting year of the funding"
     ~record_name
     ~pos:__POS__;
   lift_string_opt
     ~keyword:Public_data.Annee_Fin
     ~set_tmp:(fun state fin x -> state, {x with fin})
     ~get_tmp:(fun a -> a.fin)
     ~get:(fun a -> a.Public_data.funding_end)
     ~set:(fun funding_end a -> {a with Public_data.funding_end})
     ~field_name:"the ending year of the funding"
     ~record_name
     ~pos:__POS__;
  ]

let compute_repository =
  Remanent_state.get_scholarships_list_repository

let get_scholarships
    ?repository
    ?prefix
    ?file_name
    ?promotion
    state =
  let p =
    (fun y ->
       match promotion, y.promotion with
       | Some a, Some b -> a=b
       | None,_ | _, None -> true
    )
  in
  Scan_csv_files.collect_gen
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~p
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_scholarship
    ~empty_elt:Public_data.empty_scholarship
    ~add_elt:Remanent_state.add_scholarship
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
