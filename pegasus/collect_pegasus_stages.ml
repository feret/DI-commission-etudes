(* Collect dens candidates from the data-bases *)
type pegasus_entry =
{
  firstname: string option;
  lastname: string option;
  credits: float option;
  valide: bool option;
  commentaire: string option;
  periode: string option;
  sujet: string option;
  directeur: string option;
}

let empty_pegasus_entry =
{
  firstname = None;
  lastname = None;
  credits = None;
  valide = None;
  commentaire = None;
  periode = None;
  sujet = None;
  directeur = None;
}


let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Periode;
    Public_data.Commentaire ;
    Public_data.Sujet_du_Stage_Type_du_Sejour;
    Public_data.Directeur_de_Stage;
    Public_data.Credits;
    Public_data.Valide;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Periode ;
    Public_data.Sujet_du_Stage_Type_du_Sejour;
  ]

let event_opt = Some Profiling.Collect_pegasus_stages

let lift_string =
  (Lift.string empty_pegasus_entry Public_data.empty_stage_pegasus).Lift.safe
let lift_pred = Lift.pred_safe
let lift_string_opt =
  (Lift.string empty_pegasus_entry Public_data.empty_stage_pegasus).Lift.opt_safe
let lift_bool_opt =
  (Lift.bool empty_pegasus_entry Public_data.empty_stage_pegasus).Lift.opt_safe
  let lift_float_opt =
    (Lift.float empty_pegasus_entry Public_data.empty_stage_pegasus).Lift.opt_safe

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

  let collect_bool suffix pos state =
    Tools.collect_bool
      (fun msg state ->
         let msg = msg^suffix in
         Remanent_state.warn
           pos
           msg
           Exit
           state
      )
      state

let _ = lift_string_opt

let mandatory_fields =
  [
    lift_pred (fun a -> a.lastname) "Student's family name";
    lift_pred (fun a -> a.firstname) "Student's first name";
    lift_pred (fun a -> a.sujet) "Internship's topic";
    lift_pred (fun a -> a.periode) "Internship's time frame"
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
     ~get:(fun a -> a.Public_data.pegasus_stage_lastname)
     ~set:(fun pegasus_stage_lastname a ->
         {a with Public_data.pegasus_stage_lastname})
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
     ~get:(fun a -> a.Public_data.pegasus_stage_firstname)
     ~set:(fun pegasus_stage_firstname a -> {a with Public_data.pegasus_stage_firstname})
     ~field_name:"the first name of the student"
     ~record_name
     ~pos:__POS__;

     lift_string_opt
       ~keyword:Public_data.Periode
       ~set_tmp:(fun state periode x ->
           state,
           let periode =
             match periode with
             | Some x when String.trim x = "" -> None
             | _ -> periode
           in
           {x with periode})
       ~get_tmp:(fun a -> a.periode)
       ~get:(fun a -> a.Public_data.pegasus_stage_periode)
       ~set:(fun pegasus_stage_periode a -> {a with Public_data.pegasus_stage_periode})
       ~field_name:"the time frame of the internship"
       ~record_name
       ~pos:__POS__;

       lift_string_opt
         ~keyword:Public_data.Commentaire
         ~set_tmp:(fun state commentaire x ->
             state,
             let commentaire =
               match commentaire with
               | Some x when String.trim x = "" -> None
               | _ -> commentaire
             in
             {x with commentaire})
         ~get_tmp:(fun a -> a.commentaire)
         ~get:(fun a -> a.Public_data.pegasus_stage_commentaire)
         ~set:(fun pegasus_stage_commentaire a -> {a with Public_data.pegasus_stage_commentaire})
         ~field_name:"Comment for the internship"
         ~record_name
         ~pos:__POS__;

    lift_string_opt
          ~keyword:Public_data.Sujet_du_Stage_Type_du_Sejour
          ~set_tmp:(fun state sujet x ->
               state,
               {x with sujet})
           ~get_tmp:(fun a -> a.sujet)
           ~get:(fun a -> a.Public_data.pegasus_stage_sujet)
           ~set:(fun pegasus_stage_sujet a -> {a with Public_data.pegasus_stage_sujet})
           ~field_name:"Topic of the internship"
           ~record_name
           ~pos:__POS__;

    lift_string_opt
          ~keyword:Public_data.Directeur_de_Stage
          ~set_tmp:(fun state directeur x ->
                      state,
                      {x with directeur})
          ~get_tmp:(fun a -> a.directeur)
          ~get:(fun a -> a.Public_data.pegasus_stage_directeur)
          ~set:(fun pegasus_stage_directeur a -> {a with Public_data.pegasus_stage_directeur})
          ~field_name:"Advisor"
          ~record_name
          ~pos:__POS__;

      lift_bool_opt
        ~keyword:Public_data.Valide
        ~set_tmp:(collect_bool "in validation" __POS__ (fun valide x ->
              {x with valide}))
        ~get_tmp:(fun a -> a.valide)
        ~get:(fun a -> a.Public_data.pegasus_stage_valide)
        ~set:(fun pegasus_stage_valide a -> {a with Public_data.pegasus_stage_valide})
        ~field_name:"Validation"
        ~record_name
        ~pos:__POS__;

        lift_float_opt
            ~keyword:Public_data.Credits
            ~set_tmp:(collect_float "Credits" __POS__ (fun credits x -> {x with credits}))
            ~get_tmp:(fun a -> a.credits)
            ~get:(fun a -> a.Public_data.pegasus_stage_credits)
            ~set:(fun pegasus_stage_credits a -> {a with Public_data.pegasus_stage_credits})
            ~field_name:"Credits"
            ~record_name
            ~pos:__POS__ ;

  ]

let compute_repository =
  Remanent_state.Collector_pegasus_stages.get_repository

let get_pegasus_stages
    ?repository
    ?prefix
    ?file_name
    state =
  let p =
    (fun _ -> true)
  in
  Scan_csv_files.collect_gen
    ~strict:true
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~p
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_pegasus_entry
    ~empty_elt:Public_data.empty_stage_pegasus
    ~add_elt:Remanent_state.add_pegasus_stage
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
