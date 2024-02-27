type inscritions  =
  {
    dpt_acronym: Public_data.main_dpt option;
    annee: string option;
    niveau: string option;
    universite: Public_data.universite option;
    nom: string option;
    prenom: string option;
  }

let empty_inscription =
  {
    dpt_acronym = None;
    annee = None;
    niveau = None;
    universite = None;
    nom = None;
    prenom = None
  }

let lift_pred = Lift.pred_safe
let lift_pred_opt = Lift.pred_opt_safe
let lift_string =
  (Lift.string empty_inscription Public_data.empty_inscription).Lift.safe
let lift_dpt_opt =
  (Lift.main_dpt empty_inscription Public_data.empty_inscription).Lift.opt_safe
let lift_universite_opt =
  (Lift.universite empty_inscription Public_data.empty_inscription).Lift.opt_safe

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Departement ;
    Public_data.Annee_Academique;
    Public_data.Niveau;
    Public_data.Universite;
    Public_data.LastName;
    Public_data.FirstName;
  ]

let keywords_of_interest =
  [
    Public_data.Departement ;
    Public_data.Annee_Academique;
    Public_data.Niveau;
    Public_data.LastName;
    Public_data.FirstName;
  ]

let event_opt = Some (Profiling.Collect_inscriptions)
let compute_repository =
  Remanent_state.get_inscriptions_list_repository

let mandatory_fields =
  [
    lift_pred (fun a -> a.niveau)
      "Name of academic cursus is missing";
    lift_pred (fun a -> a.annee)
      "Year of academic cursus is missing";
    lift_pred_opt (fun a -> a.dpt_acronym)
      "Dpt of academic cursus is missing";
    lift_pred_opt (fun a -> a.universite)
      "University of academic cursus is missing";
      lift_pred_opt (fun a -> a.universite)
        "University of academic cursus is missing";
      lift_pred_opt (fun a -> a.nom)
          "Student last name is missing";
      lift_pred_opt (fun a -> a.prenom)
            "Student first name is missing";
  ]
  let fun_ignore =
    (fun state _ x -> state, x)

let all_fields =
  let record_name = "inscriptions" in
  [
    lift_string
      ~keyword:Public_data.Niveau
      ~set_tmp:(fun state niveau x ->
          state,
          let niveau =
            match niveau with
            | Some x when String.trim x = "" -> None
            | _ -> niveau
          in
          {x with niveau})
      ~get_tmp:(fun a -> a.niveau)
      ~get:(fun a -> a.Public_data.inscription_niveau)
      ~set:(fun inscription_niveau a -> {a with Public_data.inscription_niveau})
      ~field_name:"level"
      ~record_name
      ~pos:__POS__;
    lift_dpt_opt
      ~keyword:Public_data.Departement
      ~set_tmp:(fun state dpt x ->
          state,
          let dpt_acronym = Tools.map_opt Public_data.dpt_of_string dpt
          in
          {x with dpt_acronym})
      ~get_tmp:(fun a -> a.dpt_acronym)
      ~get:(fun a -> a.Public_data.inscription_dpt)
      ~set:(fun inscription_dpt a ->
               {a with Public_data.inscription_dpt})
      ~field_name:"acronym of the department"
      ~record_name
      ~pos:__POS__;
    lift_universite_opt
        ~keyword:Public_data.Universite
        ~set_tmp:(fun state univ x ->
            state,
            let universite = Tools.map_opt Public_data.univ_of_string univ
            in
            {x with universite})
        ~get_tmp:(fun a -> a.universite)
        ~get:(fun a -> a.Public_data.inscription_univ)
        ~set:(fun inscription_univ a ->
                 {a with Public_data.inscription_univ})
        ~field_name:"university"
        ~record_name
        ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(fun state annee x ->
          state,
          let annee =
            match annee with
            | Some x when String.trim x = "" -> None
            | _ -> annee
          in
          {x with annee})
      ~get_tmp:(fun a -> a.annee)
      ~get:(fun a -> a.Public_data.inscription_annee_academique)
      ~set:(fun inscription_annee_academique a ->
          {a with Public_data.inscription_annee_academique})
      ~field_name:"year of the cursus"
      ~record_name
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(fun state name x ->
          state,
          let nom =
            match name with
            | Some x when String.trim x = "" -> None
            | _ -> name
          in
          {x with nom})
      ~get_tmp:(fun a -> a.nom)
      ~get:(fun a -> a.Public_data.inscription_nom)
      ~set:(fun inscription_nom a ->
          {a with Public_data.inscription_nom})
      ~field_name:"nom"
      ~record_name
      ~pos:__POS__;
      lift_string
        ~keyword:Public_data.FirstName
        ~set_tmp:(fun state prenom x ->
            state,
            let prenom =
              match prenom with
              | Some x when String.trim x = "" -> None
              | _ -> prenom
            in
            {x with prenom})
        ~get_tmp:(fun a -> a.prenom)
        ~get:(fun a -> a.Public_data.inscription_prenom)
        ~set:(fun inscription_prenom a ->
            {a with Public_data.inscription_prenom})
        ~field_name:"prenom"
        ~record_name
        ~pos:__POS__;

  ]

let get_inscriptions
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
    ~init_state:empty_inscription
    ~empty_elt:Public_data.empty_inscription
    ~add_elt:Remanent_state.add_inscription
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
