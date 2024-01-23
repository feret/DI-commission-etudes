
type at =
  {
    at_nom: string option;
    at_prenom: string option;
    at_annee: Public_data.annee option;
    at_libelle: string option;
    at_libelle_fr: string option;
    at_libelle_en: string option;
    at_activite_fr: string option;
    at_activite_en: string option;
    at_experience: Public_data.experience option
  }

let debug=false

let empty_at=
  {
    at_nom= None;
    at_prenom= None;
    at_annee= None;
    at_libelle= None;
    at_libelle_fr= None;
    at_libelle_en= None;
    at_activite_fr= None;
    at_activite_en= None;
    at_experience= None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName ;
    Public_data.LastName ;
    Public_data.Annee_de_Validation_du_Cours;
    Public_data.Sujet_du_Stage_Type_du_Sejour ;
    Public_data.Sujet_FR;
    Public_data.Sujet_EN;
    Public_data.Activite;
    Public_data.Activite_en;
    Public_data.Experience
    ]

let keywords_of_interest =
  [
  Public_data.FirstName ;
  Public_data.LastName ;
  Public_data.Annee_de_Validation_du_Cours;
  Public_data.Sujet_du_Stage_Type_du_Sejour ;
  ]

let compute_repository =
  Remanent_state.Collector_stages_tries.get_repository

let event_opt = Some Profiling.Collect_sorted_internships

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_at  Public_data.empty_stage_a_trier).Lift.safe
  let lift_string_opt =
    (Lift.string empty_at  Public_data.empty_stage_a_trier).Lift.opt_safe
let lift_exp_opt =
    (Lift.experience empty_at Public_data.empty_stage_a_trier).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> Some a.at_nom) "Last Name";
    lift_pred (fun a -> Some a.at_prenom) "First Name";
    lift_pred (fun a -> Some a.at_libelle) "Sujet";
    lift_pred (fun a -> Some a.at_annee) "Validation year";
  ]

let all_fields =
  let record_name = "an internship attribution" in
  [
   lift_string
     ~keyword:Public_data.Sujet_du_Stage_Type_du_Sejour
     ~set_tmp:(fun state at_libelle x ->
         state, {x with at_libelle})
     ~get_tmp:(fun a -> a.at_libelle)
     ~get:(fun a -> a.Public_data.stageat_libelle)
     ~set:(fun stageat_libelle a ->
        {a with Public_data.stageat_libelle})
     ~field_name:"sujet"
     ~record_name
     ~pos:__POS__ ;
     lift_string
       ~keyword:Public_data.Sujet_FR
       ~set_tmp:(fun state at_libelle_fr x ->
           state, {x with at_libelle_fr})
       ~get_tmp:(fun a -> a.at_libelle_fr)
       ~get:(fun a -> a.Public_data.stageat_libelle_fr)
       ~set:(fun stageat_libelle_fr a ->
          {a with Public_data.stageat_libelle_fr})
       ~field_name:"sujet_fr"
       ~record_name
       ~pos:__POS__ ;
       lift_string
         ~keyword:Public_data.Sujet_EN
         ~set_tmp:(fun state at_libelle_en x ->
             state, {x with at_libelle_en})
         ~get_tmp:(fun a -> a.at_libelle_en)
         ~get:(fun a -> a.Public_data.stageat_libelle_en)
         ~set:(fun stageat_libelle_en a ->
            {a with Public_data.stageat_libelle_en})
         ~field_name:"sujet_en"
         ~record_name
         ~pos:__POS__ ;
         lift_string_opt
           ~keyword:Public_data.Activite
           ~set_tmp:(fun state at_activite_fr x ->
               state, {x with at_activite_fr})
           ~get_tmp:(fun a -> a.at_activite_fr)
           ~get:(fun a -> a.Public_data.stageat_activite_fr)
           ~set:(fun stageat_activite_fr a ->
              {a with Public_data.stageat_activite_fr})
           ~field_name:"sujet_fr"
           ~record_name
           ~pos:__POS__ ;
           lift_string_opt
             ~keyword:Public_data.Activite_en
             ~set_tmp:(fun state at_activite_en x ->
                 state, {x with at_activite_en})
             ~get_tmp:(fun a -> a.at_activite_en)
             ~get:(fun a -> a.Public_data.stageat_activite_en)
             ~set:(fun stageat_activite_en a ->
                {a with Public_data.stageat_activite_en})
             ~field_name:"sujet_en"
             ~record_name
             ~pos:__POS__ ;
     lift_string
        ~keyword:Public_data.LastName
        ~set_tmp:(fun state lastname x ->
            state,
            let at_nom =
              match lastname with
              | Some x when String.trim x = "" -> None
              | _ -> lastname
            in
            {x with at_nom})
        ~get_tmp:(fun a -> a.at_nom)
        ~get:(fun a -> a.Public_data.stageat_nom)
        ~set:(fun stageat_nom a ->
            {a with Public_data.stageat_nom})
        ~field_name:"the family name of the student"
        ~record_name
        ~pos:__POS__ ;
      lift_string
        ~keyword:Public_data.FirstName
        ~set_tmp:(fun state firstname x ->
            state,
            let at_prenom =
              match firstname with
              | Some x when String.trim x = "" -> None
              | _ -> firstname
            in
            {x with at_prenom})
        ~get_tmp:(fun a -> a.at_prenom)
        ~get:(fun a -> a.Public_data.stageat_prenom)
        ~set:(fun stageat_prenom a -> {a with Public_data.stageat_prenom = stageat_prenom})
        ~field_name:"the first name of the student"
        ~record_name
        ~pos:__POS__;
      lift_string
        ~keyword:Public_data.Annee_de_Validation_du_Cours
        ~set_tmp:(fun state at_annee x -> state, {x with at_annee})
        ~get_tmp:(fun a -> a.at_annee)
        ~get:(fun a -> a.Public_data.stageat_annee)
        ~set:(fun stageat_annee a -> {a with Public_data.stageat_annee})
        ~field_name:"the promotion year of the student"
        ~record_name
        ~pos:__POS__;


     lift_exp_opt
       ~keyword:Public_data.Experience
       ~set_tmp:(fun state at_experience x ->
         let at_experience =
           Tools.map_opt
             Public_data.experience_of_string at_experience
         in state, {x with at_experience})
       ~get_tmp:(fun a -> a.at_experience)
       ~get:(fun a -> a.Public_data.stageat_type)
       ~set:(fun stageat_type a ->
          {a with Public_data.stageat_type})
       ~field_name:"category"
       ~record_name
       ~pos:__POS__ ;
]

let get_sorted_internships
    ?repository
    ?prefix
    ?file_name
    state
  =
  Scan_csv_files.collect_gen
    ~debug
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_at
    ~empty_elt:Public_data.empty_stage_a_trier
    ~add_elt:Remanent_state.Collector_stages_tries.add
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state

let unify_sorted_internships =
  Scan_gen_files.unify_gen ~all_fields
