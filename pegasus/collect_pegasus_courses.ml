(* Collect courses from Helisa *)


type pegasus_entry =
{pegasus_helisa: string option;
pegasus_libelle: string option;
pegasus_libelle_en: string option;
pegasus_prof_prenom: string option;
pegasus_prof_nom: string option;
pegasus_codegps: string option;
pegasus_session:string option;
pegasus_semester: string option;
}

let empty_course =
{pegasus_helisa = None;
pegasus_libelle = None;
pegasus_libelle_en = None;
pegasus_prof_nom = None;
pegasus_prof_prenom = None;
pegasus_codegps = None;
pegasus_session = None;
pegasus_semester = None;
}



let event_opt = Some (Profiling.Collect_pegasus_courses)
let compute_repository = Remanent_state.Collector_course_pegasus.get_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_course Public_data.empty_course_pegasus).Lift.safe
let lift_string_opt =
    (Lift.string empty_course Public_data.empty_course_pegasus).Lift.opt_safe


let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.PEGASUS_Code_Produit_Helisa;
    Public_data.PEGASUS_Libelle;
    Public_data.PEGASUS_Libelle_Anglais;
    Public_data.PEGASUS_Type_de_produit;
    Public_data.PEGASUS_Domaine;
    Public_data.PEGASUS_Nature_du_produit;
    Public_data.PEGASUS_Niveau;
    Public_data.PEGASUS_Nature_de_l_activite;
    Public_data.PEGASUS_Resp_Adm;
    Public_data.PEGASUS_Unite;
    Public_data.PEGASUS_TVA;
    Public_data.PEGASUS_Prestation;
    Public_data.PEGASUS_Session;
    Public_data.PEGASUS_Nb_Maxi_Inscrits;
    Public_data.PEGASUS_Etat;
    Public_data.PEGASUS_Lieu;
    Public_data.PEGASUS_Date_Session;
    Public_data.PEGASUS_Date_Fin_Session;
    Public_data.PEGASUS_NB_INSCRITS_ACTIFS;
    Public_data.PEGASUS_NB_INSCRITS_CLOTURES;
    Public_data.PEGASUS_PED_NOM;
    Public_data.PEGASUS_PED_PRENOM;
    Public_data.PEGASUS_PED_EMAIL;
    Public_data.PEGASUS_RES_NOM;
    Public_data.PEGASUS_RES_PRENOM;
    Public_data.PEGASUS_RES_SOCIETE;
    Public_data.PEGASUS_SE_DEROULER_DANS_SOCIETE;
    Public_data.PEGASUS_COMPTE_COMPTABLE;
    Public_data.PEGASUS_CO_PRODUIT_EDT_COULEUR_DU_PRODUIT;
    Public_data.PEGASUS_CO_PRODUIT_ID_PAIEMENT_EN_LIGNE;
    Public_data.PEGASUS_CO_PRODUIT_EMARGER_COURS_MUTUALISE;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ETAB_NOM;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ETAB_VILLE;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ETAB_DEPARTEMENT;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_INTITULE;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ENSEIGNANT;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_01;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_02;
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_GESTIONNAIRE;
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_01;
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_02;
    Public_data.PEGASUS_CO_PRODUIT_NOTES;
    Public_data.PEGASUS_CO_PRODUIT_ID_GIROFLE;
    Public_data.PEGASUS_CO_PRODUIT_ID_PRODUIT_SF;
    Public_data.PEGASUS_CO_PRODUIT_ID_PRODUIT_SF_02;
    Public_data.PEGASUS_CO_PRODUIT_LANGUE_ENSEIGNEMENT;
    Public_data.PEGASUS_CO_ANNEE_EDT_VISIBLE_SUR_PORTAIL_JUSQUE;
    Public_data.PEGASUS_CO_ANNEE_EDT_VISIBLE_SUR_WEBPROF_JUSQUE;
    Public_data.PEGASUS_CO_ANNEE_STATUT_ACCES_DECISIONS_JURY;
    Public_data.PEGASUS_CO_ANNEE_EMARGER_FORMATION_OUVERTE;
    Public_data.PEGASUS_CO_ANNEE_EMARGER_ENSEIGNEMENT_FERME;
    Public_data.PEGASUS_CO_ANNEE_AUTORISATION_EDITION_BULLETIN;
    Public_data.PEGASUS_CO_ANNEE_AFFICHE_CLASSEMENT_EDITION;
  ]

  let keywords_of_interest =
    [
    Public_data.PEGASUS_Code_Produit_Helisa;
    Public_data.PEGASUS_Nature_de_l_activite;
    Public_data.PEGASUS_Libelle;
    Public_data.PEGASUS_Libelle_Anglais;
    Public_data.PEGASUS_PED_NOM;
    Public_data.PEGASUS_PED_PRENOM;
    Public_data.PEGASUS_CO_PRODUIT_ID_GIROFLE;
    Public_data.PEGASUS_Session;
  ]

let mandatory_fields =
      [
      lift_pred (fun a -> a.pegasus_helisa) "Code (HELISA)";
      lift_pred (fun a -> a.pegasus_libelle) "libelle" ;
      lift_pred (fun a -> a.pegasus_session) "session"
      ]

let all_fields =
    let record_name = "Courses in HELISA database" in
        [
          lift_string
            ~keyword:Public_data.PEGASUS_Code_Produit_Helisa
            ~set_tmp:(Tools.collect_string
                        (fun pegasus_helisa x -> {x with pegasus_helisa}))
            ~get_tmp:(fun a -> a.pegasus_helisa)
            ~get:(fun a -> a.Public_data.pegasus_helisa)
            ~set:(fun pegasus_helisa a -> {a with Public_data.pegasus_helisa})
            ~record_name
            ~field_name:"Code in Helisa"
            ~pos:__POS__;
          lift_string
            ~keyword:Public_data.PEGASUS_Session
            ~set_tmp:(Tools.collect_string
                          (fun pegasus_session x -> {x with pegasus_session}))
            ~get_tmp:(fun a -> a.pegasus_session)
            ~get:(fun a -> a.Public_data.pegasus_session)
            ~set:(fun pegasus_session a -> {a with Public_data.pegasus_session})
            ~record_name
            ~field_name:"Session"
            ~pos:__POS__;
          lift_string_opt
            ~keyword:Public_data.PEGASUS_CO_PRODUIT_ID_GIROFLE
            ~set_tmp:(Tools.collect_string
                          (fun pegasus_codegps x -> {x with pegasus_codegps}))
            ~get_tmp:(fun a -> a.pegasus_codegps)
            ~get:(fun a -> a.Public_data.pegasus_codegps)
            ~set:(fun pegasus_codegps a -> {a with Public_data.pegasus_codegps})
            ~record_name
            ~field_name:"Code in Girofle"
            ~pos:__POS__;
            lift_string_opt
              ~keyword:Public_data.PEGASUS_Nature_de_l_activite
              ~set_tmp:(Tools.collect_string
                            (fun pegasus_semester x -> {x with pegasus_semester}))
              ~get_tmp:(fun a -> a.pegasus_semester)
              ~get:(fun a -> a.Public_data.pegasus_semester)
              ~set:(fun pegasus_semester a -> {a with Public_data.pegasus_semester})
              ~record_name
              ~field_name:"Semester"
              ~pos:__POS__;
          lift_string_opt
            ~keyword:Public_data.PEGASUS_PED_NOM
            ~set_tmp:(Tools.collect_string
                            (fun pegasus_prof_nom x -> {x with pegasus_prof_nom}))
            ~get_tmp:(fun a -> a.pegasus_prof_nom)
            ~get:(fun a -> a.Public_data.pegasus_prof_nom)
            ~set:(fun pegasus_prof_nom a -> {a with Public_data.pegasus_prof_nom})
            ~record_name
            ~field_name:"Professor name"
            ~pos:__POS__;
            lift_string_opt
              ~keyword:Public_data.PEGASUS_PED_PRENOM
              ~set_tmp:(Tools.collect_string
                              (fun pegasus_prof_prenom x -> {x with pegasus_prof_prenom}))
              ~get_tmp:(fun a -> a.pegasus_prof_prenom)
              ~get:(fun a -> a.Public_data.pegasus_prof_prenom)
              ~set:(fun pegasus_prof_prenom a -> {a with Public_data.pegasus_prof_prenom})
              ~record_name
              ~field_name:"Professor name"
              ~pos:__POS__;
          lift_string_opt
            ~keyword:Public_data.PEGASUS_Libelle_Anglais
            ~set_tmp:(Tools.collect_string
                              (fun pegasus_libelle_en x -> {x with pegasus_libelle_en}))
            ~get_tmp:(fun a -> a.pegasus_libelle_en)
            ~get:(fun a -> a.Public_data.pegasus_libelle_en)
            ~set:(fun pegasus_libelle_en a -> {a with Public_data.pegasus_libelle_en})
            ~record_name
            ~field_name:"Libelle (Anglais)"
            ~pos:__POS__;
            lift_string
              ~keyword:Public_data.PEGASUS_Libelle
              ~set_tmp:(Tools.collect_string
                                (fun pegasus_libelle x -> {x with pegasus_libelle}))
              ~get_tmp:(fun a -> a.pegasus_libelle)
              ~get:(fun a -> a.Public_data.pegasus_libelle)
              ~set:(fun pegasus_libelle a ->
                  {a with Public_data.pegasus_libelle})
              ~record_name
              ~field_name:"Libelle (Français)"
              ~pos:__POS__;
(*    Public_data.PEGASUS_Libelle;
            Public_data.PEGASUS_Libelle_Anglais;
            Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ENSEIGNANT;
        *)
  (*        lift_string
            ~keyword:Public_data.PEGASUS_PRENOM
            ~set_tmp:(Tools.collect_string
                        (fun pegasus_firstname x ->
                              {x with pegasus_firstname}))
            ~get_tmp:(fun a -> a.pegasus_firstname)
            ~get:(fun a -> a.Public_data.pegasus_firstname)
            ~set:(fun pegasus_firstname a ->
               {a with Public_data.pegasus_firstname})
            ~record_name
            ~field_name:"first name of the student"
            ~pos:__POS__ ;
            lift_string
              ~keyword:Public_data.PEGASUS_NOM
              ~set_tmp:(Tools.collect_string
                          (fun pegasus_lastname x ->
                                {x with pegasus_lastname}))
              ~get_tmp:(fun a -> a.pegasus_lastname)
              ~get:(fun a -> a.Public_data.pegasus_lastname)
              ~set:(fun pegasus_lastname a ->
                 {a with Public_data.pegasus_lastname})
              ~record_name
              ~field_name:"last name of the student"
              ~pos:__POS__ ;
              lift_string
                ~keyword:Public_data.PEGASUS_ENS_PROMO
                ~set_tmp:(Tools.collect_string
                            (fun pegasus_promotion x ->
                                  {x with pegasus_promotion}))
                ~get_tmp:(fun a -> a.pegasus_promotion)
                ~get:(fun a -> a.Public_data.pegasus_promotion)
                ~set:(fun pegasus_promotion a ->
                   {a with Public_data.pegasus_promotion})
                ~record_name
                ~field_name:"promotion of the student"
                ~pos:__POS__ ;
                lift_string
                  ~keyword:Public_data.PEGASUS_NAISSANCE_VILLE
                  ~set_tmp:(Tools.collect_string
                              (fun pegasus_birth_city_fr x ->
                                    {x with pegasus_birth_city_fr}))
                  ~get_tmp:(fun a -> a.pegasus_birth_city_fr)
                  ~get:(fun a -> a.Public_data.pegasus_birth_city_fr)
                  ~set:(fun pegasus_birth_city_fr a ->
                     {a with Public_data.pegasus_birth_city_fr})
                  ~record_name
                  ~field_name:"city of birth of the student"
                  ~pos:__POS__ ;
                  lift_string
                    ~keyword:Public_data.PEGASUS_NAISSANCE_PAYS
                    ~set_tmp:(Tools.collect_string
                                (fun pegasus_birth_country_fr x ->
                                      {x with pegasus_birth_country_fr}))
                    ~get_tmp:(fun a -> a.pegasus_birth_country_fr)
                    ~get:(fun a -> a.Public_data.pegasus_birth_country_fr)
                    ~set:(fun pegasus_birth_country_fr a ->
                       {a with Public_data.pegasus_birth_country_fr})
                    ~record_name
                    ~field_name:"country of birth of the student"
                    ~pos:__POS__ ;
                    lift_string
                      ~keyword:Public_data.PEGASUS_NUMERO_INE
                      ~set_tmp:(Tools.collect_string
                                  (fun pegasus_ine x ->
                                        {x with pegasus_ine}))
                      ~get_tmp:(fun a -> a.pegasus_ine)
                      ~get:(fun a -> a.Public_data.pegasus_ine)
                      ~set:(fun pegasus_ine a ->
                         {a with Public_data.pegasus_ine})
                      ~record_name
                      ~field_name:"ine number of the student"
                      ~pos:__POS__ ;
                      lift_string
                        ~keyword:Public_data.PEGASUS_PRODUIT_CODE
                        ~set_tmp:(Tools.collect_string
                                    (fun pegasus_produit_de_formation x ->
                                          {x with pegasus_produit_de_formation}))
                        ~get_tmp:(fun a -> a.pegasus_produit_de_formation)
                        ~get:(fun a -> a.Public_data.pegasus_produit_de_formation)
                        ~set:(fun pegasus_produit_de_formation a ->
                           {a with Public_data.pegasus_produit_de_formation})
                        ~record_name
                        ~field_name:"Formation number of the student"
                        ~pos:__POS__ ;
(*

              Public_data.PEGASUS_ine_NUMERO;
              Public_data.PEGASUS_NAISSANCE_VILLE;
              Public_data.PEGASUS_NAISSANCE_PAYS*)

*)
                    ]

        let get_pegasus_courses
            ?repository
            ?prefix
            ?file_name
            state
            =
            let state, str = compute_repository state in
            let event = Some (Profiling.Scan_csv_files (str,"")) in
            let state = Remanent_state.open_event_opt event state in
            let state = Scan_xlss_files.collect_gen
              (* ~debug:true *)
              ~strict:false
              ?repository
              ?prefix
              ?file_name
              ~compute_repository
              ~fun_default:Tools.fun_ignore
              ~keywords_of_interest
              ~keywords_list
              ~init_state:empty_course
              ~empty_elt:Public_data.empty_course_pegasus ~add_elt:Remanent_state.Collector_course_pegasus.add
              ~mandatory_fields
              ~all_fields
              ?event_opt
              state
            in
            let state = Remanent_state.close_event_opt event state in
            state
