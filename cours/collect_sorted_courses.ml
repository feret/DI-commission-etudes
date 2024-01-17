
type at =
  {
    at_nom: string option;
    at_prenom: string option;
    at_annee: Public_data.annee option;
    at_libelle: string option;
    at_dpt: Public_data.main_dpt option;
    at_codegps: string option;

  }


let empty_at=
  {
    at_nom=None;
    at_prenom=None;
    at_annee=None;
    at_libelle=None;
    at_dpt=None;
    at_codegps=None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName ;
    Public_data.LastName ;
    Public_data.Departement;
    Public_data.Annee_de_Validation_du_Cours;
    Public_data.Libelle ;
  ]

let keywords_of_interest =
  [
  Public_data.FirstName ;
  Public_data.LastName ;
  Public_data.Annee_de_Validation_du_Cours;
  Public_data.Libelle ;
  Public_data.Code_gps ;
  ]

let compute_repository =
  Remanent_state.get_sorted_courses_list_repository

let event_opt = Some Profiling.Collect_sorted_courses

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_at  Public_data.empty_cours_a_trier).Lift.safe

  let lift_dpt_opt =
    (Lift.main_dpt empty_at Public_data.empty_cours_a_trier).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> Some a.at_nom) "Last Name";
    lift_pred (fun a -> Some a.at_prenom) "First Name";
    lift_pred (fun a -> Some a.at_libelle) "Course";
    lift_pred (fun a -> Some a.at_codegps) "GPS CODE";
    lift_pred (fun a -> Some a.at_annee) "Validation year";
  ]

let all_fields =
  let record_name = "a course attribution" in
  [lift_string
     ~keyword:Public_data.Code_gps
     ~set_tmp:(fun state at_codegps x -> state, {x with at_codegps})
     ~get_tmp:(fun a -> a.at_codegps)
     ~get:(fun a -> a.Public_data.coursat_codegps)
     ~set:(fun coursat_codegps a ->
         {a with Public_data.coursat_codegps})
     ~field_name:"gps code"
     ~record_name
     ~pos:__POS__;

   lift_string
     ~keyword:Public_data.Libelle
     ~set_tmp:(fun state at_libelle x ->
         state, {x with at_libelle})
     ~get_tmp:(fun a -> a.at_libelle)
     ~get:(fun a -> a.Public_data.coursat_libelle)
     ~set:(fun coursat_libelle a ->
        {a with Public_data.coursat_libelle})
     ~field_name:"course"
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
        ~get:(fun a -> a.Public_data.coursat_nom)
        ~set:(fun coursat_nom a ->
            {a with Public_data.coursat_nom})
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
        ~get:(fun a -> a.Public_data.coursat_prenom)
        ~set:(fun coursat_prenom a -> {a with Public_data.coursat_prenom = coursat_prenom})
        ~field_name:"the first name of the student"
        ~record_name
        ~pos:__POS__;
      lift_string
        ~keyword:Public_data.Annee_de_Validation_du_Cours
        ~set_tmp:(fun state at_annee x -> state, {x with at_annee})
        ~get_tmp:(fun a -> a.at_annee)
        ~get:(fun a -> a.Public_data.coursat_annee)
        ~set:(fun coursat_annee a -> {a with Public_data.coursat_annee})
        ~field_name:"the promotion year of the student"
        ~record_name
        ~pos:__POS__;


     lift_dpt_opt
       ~keyword:Public_data.Departement
       ~set_tmp:(fun state at_dpt x ->
         let at_dpt =
           Tools.map_opt
             Public_data.dpt_of_string at_dpt
         in state, {x with at_dpt})
       ~get_tmp:(fun a -> a.at_dpt)
       ~get:(fun a -> a.Public_data.coursat_dpt)
       ~set:(fun coursat_dpt a ->
          {a with Public_data.coursat_dpt})
       ~field_name:"course"
       ~record_name
       ~pos:__POS__ ;
]

let get_sorted_courses
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
    ~init_state:empty_at
    ~empty_elt:Public_data.empty_cours_a_trier
    ~add_elt:Remanent_state.add_sorted_course
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state

let unify_sorted_courses =
  Scan_gen_files.unify_gen ~all_fields
