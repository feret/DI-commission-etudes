type decision_id  =
  {
    firstname: string option;
    lastname: string option;
    decision: string option;
    annee: string option;
    program: string option;
    dpt: string option;
    mean: float option;
    mention: string option;
    rank: int option;
    effectif: int option;
    date: string option;
    commission: string option;
    validated: bool option;
  }

let empty_decision =
  {
    firstname=None;
    lastname=None;
    program=None;
    decision=None; 
    dpt=None;
    annee=None;
    mean=None;
    mention=None;
    rank=None;
    effectif=None;
    date=None;
    commission=None;
    validated=None;
  }

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Commission;
    Public_data.Date;
    Public_data.Decision;
    Public_data.Annee_Academique;
    Public_data.Classement;
    Public_data.Effectif;
    Public_data.Moyenne;
    Public_data.Mention;
    Public_data.Recu;
    Public_data.Departement;
    Public_data.Diplome;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Diplome;
    Public_data.Departement;
  ]

let collect_int suffix pos state =
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

let event_opt = Some (Profiling.Collect_decisions)
let compute_repository = Remanent_state.get_decisions_list_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_decision Public_data.empty_decision).Lift.safe
let lift_string_opt =
  (Lift.string empty_decision Public_data.empty_decision).Lift.opt_safe
let lift_bool_opt =
  (Lift.bool empty_decision Public_data.empty_decision).Lift.opt_safe
let lift_float_opt =
  (Lift.float empty_decision Public_data.empty_decision).Lift.opt_safe
let lift_int_opt =
  (Lift.int empty_decision Public_data.empty_decision).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.firstname) "the first name of the student";
    lift_pred (fun a -> a.lastname) "the last name of the student";
    lift_pred (fun a -> a.dpt) "the department";
    lift_pred (fun a -> a.program) "the name of the program";
  ]

let all_fields =
  let record_name = "a jury decision" in
  [
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(Tools.collect_string (fun firstname x -> {x with firstname}))
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.decision_firstname)
      ~set:(fun decision_firstname a ->
         {a with Public_data.decision_firstname})
      ~record_name
      ~field_name:"first name of the student"
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(Tools.collect_string (fun lastname x -> {x with lastname}))
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.decision_lastname)
      ~set:(fun decision_lastname a ->
          {a with Public_data.decision_lastname})
      ~record_name
      ~field_name:"last name of the student"
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(Tools.collect_string (fun annee x -> {x with annee}))
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.decision_annee)
      ~set:(fun decision_annee a ->
          {a with Public_data.decision_annee})
      ~record_name
      ~field_name:"year of the program"
      ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Decision
      ~set_tmp:(Tools.collect_string
                  (fun decision x -> {x with decision}))
      ~get_tmp:(fun a -> a.decision)
      ~get:(fun a -> a.Public_data.decision_decision)
      ~set:(fun decision_decision a ->
          {a with Public_data.decision_decision})
      ~record_name
      ~field_name:"decision of the jury"
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Diplome
      ~set_tmp:(Tools.collect_string
                  (fun program x -> {x with program}))
      ~get_tmp:(fun a -> a.program)
      ~get:(fun a -> a.Public_data.decision_program)
      ~set:(fun decision_program a ->
          {a with Public_data.decision_program})
      ~record_name
      ~field_name:"name of the program"
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Departement
      ~set_tmp:(Tools.collect_string (fun dpt x -> {x with dpt}))
      ~get_tmp:(fun a -> a.dpt)
      ~get:(fun a -> a.Public_data.decision_dpt)
      ~set:(fun decision_dpt a ->
          {a with Public_data.decision_dpt})
      ~record_name
      ~field_name:"department of the program"
      ~pos:__POS__ ;
    lift_string_opt
      ~keyword:Public_data.Mention
      ~set_tmp:(Tools.collect_string (fun mention x -> {x with mention}))
      ~get_tmp:(fun a -> a.mention)
      ~get:(fun a -> a.Public_data.decision_mention)
      ~set:(fun decision_mention a ->
          {a with Public_data.decision_mention})
      ~field_name:"mention"
      ~record_name
      ~pos:__POS__ ;
    lift_string_opt
      ~keyword:Public_data.Date
      ~set_tmp:(Tools.collect_string (fun date x -> {x with date}))
      ~get_tmp:(fun a -> a.date)
      ~get:(fun a -> a.Public_data.decision_date)
      ~set:(fun decision_date a ->
              {a with Public_data.decision_date})
      ~field_name:"date"
      ~record_name
      ~pos:__POS__ ;
  lift_string_opt
      ~keyword:Public_data.Commission
      ~set_tmp:(Tools.collect_string (fun commission x -> {x with commission}))
      ~get_tmp:(fun a -> a.commission)
      ~get:(fun a -> a.Public_data.decision_commission_name)
      ~set:(fun decision_commission_name a ->
          {a with Public_data.decision_commission_name})
      ~field_name:"commission name"
      ~record_name
      ~pos:__POS__ ;
    lift_bool_opt
      ~keyword:Public_data.Recu
      ~set_tmp:(collect_bool "in validation" __POS__
                  (fun validated x -> {x with validated}))
      ~get_tmp:(fun a -> a.validated)
      ~get:(fun a -> a.Public_data.decision_validated)
      ~set:(fun decision_validated a ->
          {a with Public_data.decision_validated})
      ~field_name:"validation"
      ~record_name
      ~pos:__POS__;
  lift_float_opt
      ~keyword:Public_data.Moyenne
      ~set_tmp:(collect_float "in mean" __POS__ (fun mean x -> {x with mean}))
      ~get_tmp:(fun a -> a.mean)
      ~get:(fun a -> a.Public_data.decision_mean)
      ~set:(fun decision_mean a -> {a with Public_data.decision_mean})
      ~field_name:"mean"
      ~record_name
      ~pos:__POS__ ;
    lift_int_opt
      ~keyword:Public_data.Classement
      ~set_tmp:(collect_int "in classement" __POS__ (fun rank x -> {x with rank}))
      ~get_tmp:(fun a -> a.rank)
      ~get:(fun a -> a.Public_data.decision_rank)
      ~set:(fun decision_rank a -> {a with Public_data.decision_rank})
      ~field_name:"rank"
      ~record_name
      ~pos:__POS__;
    lift_int_opt
      ~keyword:Public_data.Effectif
      ~set_tmp:(collect_int "in program effectif" __POS__ (fun effectif x -> {x with effectif}))
      ~get_tmp:(fun a -> a.effectif)
      ~get:(fun a -> a.Public_data.decision_effectif)
      ~set:(fun decision_effectif a ->
          {a with Public_data.decision_effectif})
      ~field_name:"effectif"
      ~record_name
      ~pos:__POS__;
      ]

let get_decisions
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
    ~init_state:empty_decision
    ~empty_elt:Public_data.empty_decision
    ~add_elt:Remanent_state.add_decision
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
