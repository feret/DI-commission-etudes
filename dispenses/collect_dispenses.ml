type dispense_id  =
  {
    firstname: string option;
    lastname: string option;
    annee: string option;
    motif: string option;
    motif_en: string option;
    program: string option;
    dpt: string option;
  }

let empty_dispense =
  {
    firstname=None;
    lastname=None;
    annee=None;
    dpt=None;
    motif=None;
    motif_en=None;
    program=None;
  }

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Departement;
    Public_data.Motif;
    Public_data.Motif_en;
    Public_data.Programme;
  ]

let keywords_of_interest =
  [
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Departement;
    Public_data.Motif;
    Public_data.Motif_en;
    Public_data.Programme;
  ]

let event_opt = Some (Profiling.Collect_dispenses)
let compute_repository = Remanent_state.get_dispenses_list_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_dispense Public_data.empty_dispense).Lift.safe
let lift_string_opt =
  (Lift.string empty_dispense Public_data.empty_dispense).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.firstname)  "the first name of the student";
    lift_pred (fun a -> a.lastname)  "the last name of the student";
    lift_pred (fun a -> a.dpt)  "the department";
    lift_pred (fun a -> a.program)  "the name of the program";
    lift_pred (fun a -> a.annee)  "academic year";
  ]

let all_fields =
  let record_name = "a dispense diploma" in
  [
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(
        Tools.collect_string
          (fun firstname x -> {x with firstname}))
      ~get_tmp:(fun a -> a.firstname)
      ~get:(fun a -> a.Public_data.dispense_firstname)
      ~set:(fun dispense_firstname a ->
         {a with Public_data.dispense_firstname})
      ~field_name:"the first name of the student"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(Tools.collect_string
                  (fun lastname x -> {x with lastname}))
      ~get_tmp:(fun a -> a.lastname)
      ~get:(fun a -> a.Public_data.dispense_lastname)
      ~set:(fun dispense_lastname a ->
          {a with Public_data.dispense_lastname})
      ~field_name:"the last name of the student"
      ~record_name
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(
        Tools.collect_string
          (fun annee x -> {x with annee}))
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.dispense_annee)
      ~set:(fun dispense_annee a ->
          {a with Public_data.dispense_annee})
      ~field_name:"academic year"
      ~record_name
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Departement
      ~set_tmp:(Tools.collect_string
                (fun dpt x -> {x with dpt}))
      ~get_tmp:(fun a -> a.dpt)
      ~get:(fun a -> a.Public_data.dispense_dpt)
      ~set:(fun dispense_dpt a ->
          {a with Public_data.dispense_dpt})
      ~field_name:"program department"
      ~record_name
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Programme
      ~set_tmp:(Tools.collect_string
                  (fun program x -> {x with program}))
        ~get_tmp:(fun a -> a.program)
        ~get:(fun a -> a.Public_data.dispense_program)
        ~set:(fun dispense_program a ->
            {a with Public_data.dispense_program})
        ~field_name:"program"
        ~record_name
        ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Motif
      ~set_tmp:(Tools.collect_string
                      (fun motif x -> {x with motif}))
            ~get_tmp:(fun a -> a.motif)
            ~get:(fun a -> a.Public_data.dispense_motif)
            ~set:(fun dispense_motif a ->
                {a with Public_data.dispense_motif})
            ~field_name:"raison"
            ~record_name
            ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Motif_en
      ~set_tmp:(Tools.collect_string
                  (fun motif_en x -> {x with motif_en}))
      ~get_tmp:(fun a -> a.motif_en)
      ~get:(fun a -> a.Public_data.dispense_motif_en)
      ~set:(fun dispense_motif_en a ->
          {a with Public_data.dispense_motif_en})
      ~field_name:"raison (anglais)"
      ~record_name
      ~pos:__POS__
  ]

let get_dispenses
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
    ~init_state:empty_dispense
    ~empty_elt:Public_data.empty_dispense
    ~add_elt:Remanent_state.add_dispense
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
