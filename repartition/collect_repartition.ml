type repartition_id  =
{ 
  annee: string option; 
  cours: string option; 
  code_gps: string option; 
  code_helisa: string option; 
  code_moodle: string option; 
  niveau: string option; 
  total_heures: float Public_data.or_unknown option; 
  total_cm: float Public_data.or_unknown option; 
  total_td: float Public_data.or_unknown option; 
  total_tp: float Public_data.or_unknown option; 
  ects: float option; 
  groupes_td: int option; 
  groupes_tp: int option; 
  lastname: string option; 
  firstname: string option; 
  cm:  float Public_data.or_unknown option; 
  td: float Public_data.or_unknown option; 
  tp: float Public_data.or_unknown option; 
  remuneration: Public_data.contract Public_data.or_unknown  option 
}

let _do_it a = 
  a.remuneration, a.tp, a.td, a.cm, a.groupes_td, a.groupes_tp, a.total_heures, a.total_cm, a.total_td, a.total_tp, a.ects, a.niveau, a.code_moodle, a.code_gps, a.code_helisa  


let empty_repartition =
  {
  annee=None; 
  cours=None; 
  code_gps=None; 
  code_helisa=None; 
  code_moodle=None; 
  niveau=None; 
  total_heures=None; 
  total_cm=None;  
  total_td=None; 
  total_tp=None; 
  ects=None; 
  groupes_td=None; 
  groupes_tp=None; 
  lastname=None; 
  firstname=None; 
  cm=None; 
  td=None; 
  tp=None; 
  remuneration=None; 
  }

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Commentaire ; 
    Public_data.Annee_Academique ; 
    Public_data.Name ;  
    Public_data.Code_gps ; 
    Public_data.Code_helisa ; 
    Public_data.Code_moodle ; 
    Public_data.Niveau ; 
    Public_data.Total_heure ; 
    Public_data.Total_CM ; 
    Public_data.Total_td ; 
    Public_data.Total_tp ; 
    Public_data.Groupes_td ; 
    Public_data.Groupes_tp ; 
    Public_data.ECTS ; 
    Public_data.PEGASUS_CM; 
    Public_data.PEGASUS_TD; 
    Public_data.PEGASUS_TP; 
    Public_data.Remuneration; 
  ]
    

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Annee_Academique ; 
    Public_data.Name ; 
  ]

let _collect_int suffix pos state =
  Tools.collect_int
    (fun msg state ->
       let msg = msg^suffix in
       Remanent_state.warn
         pos
         msg
         Exit
         state
    )
    state

let _collect_float suffix pos state =
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

let _collect_bool suffix pos state =
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


let float_or_unknown_of_string state t =
  if Tools.space_only t
  then state, None
  else
    let t = Tools.remove_comma t in
    try
      state, Some (Public_data.Known (float_of_string t))  
    with
    | _ ->
      try
        state, Some (Public_data.Known (float_of_int (int_of_string t)))
      with
      | _ ->
        state, Some Public_data.Not_known

let mission_or_unknown_of_string state t =
  if Tools.space_only t
  then state, None
  else
    let t = Tools.remove_comma t in
    match Public_data.contract_of_string t with 
    | None ->  state, Some Public_data.Not_known
    | Some a -> state, Some (Public_data.Known a)
     

let event_opt = Some (Profiling.Collect_decisions)
let compute_repository = 
  Remanent_state.get_charge_repository 

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_repartition Public_data.empty_pedagogical_charge).Lift.safe
let lift_string_opt =
  (Lift.string empty_repartition Public_data.empty_pedagogical_charge).Lift.opt_safe
let _lift_bool_opt =
  (Lift.bool empty_repartition Public_data.empty_pedagogical_charge).Lift.opt_safe
let _lift_float_opt =
  (Lift.float empty_repartition Public_data.empty_pedagogical_charge).Lift.opt_safe
let _lift_int_opt =
  (Lift.int empty_repartition Public_data.empty_pedagogical_charge).Lift.opt_safe
let lift_float_or_unknown_opt = 
  (Lift.or_unknown_float empty_repartition Public_data.empty_pedagogical_charge).Lift.opt_safe 
  let lift_mission_or_unknown_opt = 
  (Lift.or_unknown_mission empty_repartition Public_data.empty_pedagogical_charge).Lift.opt_safe 

let mandatory_fields =
  [
    lift_pred (fun a -> a.annee) "academic year";
    lift_pred (fun a -> a.firstname) "the first name of the teacher";
    lift_pred (fun a -> a.lastname) "the last name of the teacher";
    lift_pred (fun a -> a.cours) "the name of the course";
  ]

let all_fields =
  let record_name = "Charge d'enseignement" in
  [
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(Tools.collect_string (fun firstname x -> {x with firstname}))
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.charge_firstname)
      ~set:(fun charge_firstname a ->
         {a with Public_data.charge_firstname})
      ~record_name
      ~field_name:"first name of the teacher"
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(Tools.collect_string (fun lastname x -> {x with lastname}))
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.charge_lastname)
      ~set:(fun charge_lastname a ->
         {a with Public_data.charge_lastname})
      ~record_name
      ~field_name:"last name of the teacher"
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(Tools.collect_string (fun annee x -> {x with annee}))
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.charge_attribution_year)
      ~set:(fun charge_attribution_year a ->
          {a with Public_data.charge_attribution_year})
      ~record_name
      ~field_name:"year of the teaching duty"
      ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Name 
      ~set_tmp:(Tools.collect_string
                  (fun cours x -> {x with cours}))
      ~get_tmp:(fun a -> a.cours) 
      ~get:(fun a -> a.Public_data.charge_course_title)
      ~set:(fun charge_course_title a ->
          {a with Public_data.charge_course_title})
      ~record_name
      ~field_name:"name of the course"
      ~pos:__POS__;
   lift_float_or_unknown_opt
      ~keyword:Public_data.PEGASUS_CM 
      ~set_tmp:(fun state string_opt x -> 
                  match string_opt with 
                    | None -> state, x
                    | Some f -> 
                      let state, cm = float_or_unknown_of_string state f in 
                      state, {x with cm}) 
      ~get_tmp:(fun a -> a.cm) 
      ~get:(fun a -> a.Public_data.charge_cm)
      ~set:(fun charge_cm a ->
          {a with Public_data.charge_cm})
      ~record_name
      ~field_name:"number of cm hours"
      ~pos:__POS__;
      lift_float_or_unknown_opt
      ~keyword:Public_data.PEGASUS_TD
      ~set_tmp:(fun state string_opt x -> 
                  match string_opt with 
                    | None -> state, x
                    | Some f -> 
                      let state, td = float_or_unknown_of_string state f in 
                      state, {x with td}) 
      ~get_tmp:(fun a -> a.td) 
      ~get:(fun a -> a.Public_data.charge_td)
      ~set:(fun charge_td a ->
          {a with Public_data.charge_td})
      ~record_name
      ~field_name:"number of directed work hours"
      ~pos:__POS__;
      lift_float_or_unknown_opt
      ~keyword:Public_data.PEGASUS_TP 
      ~set_tmp:(fun state string_opt x -> 
                  match string_opt with 
                    | None -> state, x
                    | Some f -> 
                      let state, tp = float_or_unknown_of_string state f in 
                      state, {x with tp}) 
      ~get_tmp:(fun a -> a.tp) 
      ~get:(fun a -> a.Public_data.charge_tp)
      ~set:(fun charge_tp a ->
          {a with Public_data.charge_tp})
      ~record_name
      ~field_name:"number of practical work hours"
      ~pos:__POS__;
       lift_mission_or_unknown_opt
      ~keyword:Public_data.Remuneration
      ~set_tmp:(fun state string_opt x -> 
                  match string_opt with 
                    | None -> state, x
                    | Some f -> 
                      let state, remuneration = mission_or_unknown_of_string state f in 
                      state, {x with remuneration}) 
      ~get_tmp:(fun a -> a.remuneration) 
      ~get:(fun a -> a.Public_data.charge_remuneration)
      ~set:(fun charge_remuneration a ->
          {a with Public_data.charge_remuneration})
      ~record_name
      ~field_name:"type of contract"
      ~pos:__POS__;

(*    lift_string_opt
        ~keyword:Public_data.charge_en
        ~set_tmp:(Tools.collect_string
                    (fun charge_en x -> {x with charge_en}))
        ~get_tmp:(fun a -> a.charge_en)
        ~get:(fun a -> a.Public_data.charge_charge_en)
        ~set:(fun charge_charge_en a ->
            {a with Public_data.charge_charge_en})
        ~record_name
        ~field_name:"decision of the jury (english)"
        ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Diplome
      ~set_tmp:(Tools.collect_string
                  (fun program x -> {x with program}))
      ~get_tmp:(fun a -> a.program)
      ~get:(fun a -> a.Public_data.charge_program)
      ~set:(fun charge_program a ->
          {a with Public_data.charge_program})
      ~record_name
      ~field_name:"name of the program"
      ~pos:__POS__;
    lift_dpt
      ~keyword:Public_data.Departement
      ~set_tmp:(Tools.collect_string (fun dpt x ->
          let dpt =
            Tools.map_opt
              Public_data.dpt_of_string dpt
          in {x with dpt}))
      ~get_tmp:(fun a -> a.dpt)
      ~get:(fun a -> a.Public_data.charge_dpt)
      ~set:(fun charge_dpt a ->
          {a with Public_data.charge_dpt})
      ~record_name
      ~field_name:"department of the program"
      ~pos:__POS__ ;
    lift_string_opt
      ~keyword:Public_data.Mention
      ~set_tmp:(Tools.collect_string (fun mention x -> {x with mention}))
      ~get_tmp:(fun a -> a.mention)
      ~get:(fun a -> a.Public_data.charge_mention)
      ~set:(fun charge_mention a ->
          {a with Public_data.charge_mention})
      ~field_name:"mention"
      ~record_name
      ~pos:__POS__ ;
      lift_string_opt
        ~keyword:Public_data.Mention_en
        ~set_tmp:(Tools.collect_string (fun mention_en x -> {x with mention_en}))
        ~get_tmp:(fun a -> a.mention_en)
        ~get:(fun a -> a.Public_data.charge_mention_en)
        ~set:(fun charge_mention_en a ->
            {a with Public_data.charge_mention_en})
        ~field_name:"mention (english)"
        ~record_name
        ~pos:__POS__ ;
    lift_string_opt
      ~keyword:Public_data.Date
      ~set_tmp:(Tools.collect_string (fun date x -> {x with date}))
      ~get_tmp:(fun a -> a.date)
      ~get:(fun a -> a.Public_data.charge_date)
      ~set:(fun charge_date a ->
              {a with Public_data.charge_date})
      ~field_name:"date"
      ~record_name
      ~pos:__POS__ ;
      lift_string_opt
        ~keyword:Public_data.Date_en
        ~set_tmp:(Tools.collect_string (fun date_en x -> {x with date_en}))
        ~get_tmp:(fun a -> a.date_en)
        ~get:(fun a -> a.Public_data.charge_date_en)
        ~set:(fun charge_date_en a ->
                {a with Public_data.charge_date_en})
        ~field_name:"date(english)"
        ~record_name
        ~pos:__POS__ ;
  lift_string_opt
      ~keyword:Public_data.Commission
      ~set_tmp:(Tools.collect_string (fun commission x -> {x with commission}))
      ~get_tmp:(fun a -> a.commission)
      ~get:(fun a -> a.Public_data.charge_commission_name)
      ~set:(fun charge_commission_name a ->
          {a with Public_data.charge_commission_name})
      ~field_name:"commission name"
      ~record_name
      ~pos:__POS__ ;
      lift_string_opt
          ~keyword:Public_data.Commission_en
          ~set_tmp:(Tools.collect_string (fun commission_en x -> {x with commission_en}))
          ~get_tmp:(fun a -> a.commission_en)
          ~get:(fun a -> a.Public_data.charge_commission_name_en)
          ~set:(fun charge_commission_name_en a ->
              {a with Public_data.charge_commission_name_en})
          ~field_name:"commission name (english)"
          ~record_name
          ~pos:__POS__ ;
    lift_bool_opt
      ~keyword:Public_data.Recu
      ~set_tmp:(collect_bool "in validation" __POS__
                  (fun validated x -> {x with validated}))
      ~get_tmp:(fun a -> a.validated)
      ~get:(fun a -> a.Public_data.charge_validated)
      ~set:(fun charge_validated a ->
          {a with Public_data.charge_validated})
      ~field_name:"validation"
      ~record_name
      ~pos:__POS__;
  lift_float_opt
      ~keyword:Public_data.Moyenne
      ~set_tmp:(collect_float "in mean" __POS__ (fun mean x -> {x with mean}))
      ~get_tmp:(fun a -> a.mean)
      ~get:(fun a -> a.Public_data.charge_mean)
      ~set:(fun charge_mean a -> {a with Public_data.charge_mean})
      ~field_name:"mean"
      ~record_name
      ~pos:__POS__ ;
    lift_int_opt
      ~keyword:Public_data.Classement
      ~set_tmp:(collect_int "in classement" __POS__ (fun rank x -> {x with rank}))
      ~get_tmp:(fun a -> a.rank)
      ~get:(fun a -> a.Public_data.charge_rank)
      ~set:(fun charge_rank a -> {a with Public_data.charge_rank})
      ~field_name:"rank"
      ~record_name
      ~pos:__POS__;
    lift_int_opt
      ~keyword:Public_data.Effectif
      ~set_tmp:(collect_int "in program effectif" __POS__ (fun effectif x -> {x with effectif}))
      ~get_tmp:(fun a -> a.effectif)
      ~get:(fun a -> a.Public_data.charge_effectif)
      ~set:(fun charge_effectif a ->
          {a with Public_data.charge_effectif})
      ~field_name:"effectif"
      ~record_name
      ~pos:__POS__;*)
      ]

let get_pedagogical_charges
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
    ~init_state:empty_repartition
    ~empty_elt:Public_data.empty_pedagogical_charge
    ~add_elt:Remanent_state.add_pedagogical_charge
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
