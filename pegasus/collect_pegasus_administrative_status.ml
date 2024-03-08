(* Collect dens candidates from the data-bases *)
type pegasus_entry =
{pegasus_firstname: string option;
pegasus_lastname: string option;
pegasus_promotion: string option;
pegasus_birth_country_fr: string option;
pegasus_birth_city_fr: string option;
pegasus_produit_de_formation: string option;
pegasus_ine: string option;
pegasus_origin: string option;
pegasus_birthdate: string option;
pegasus_gender: string option;}

let empty_candidate_id =
{pegasus_firstname=None;
pegasus_lastname=None;
pegasus_promotion=None;
pegasus_birth_country_fr=None;
pegasus_birth_city_fr=None;
pegasus_produit_de_formation=None;
pegasus_ine=None;
pegasus_origin=None;
pegasus_birthdate=None;
pegasus_gender=None;}



let event_opt = Some (Profiling.Collect_pegasus_data)
let compute_repository = Remanent_state.Collector_administrative_status.get_repository

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_candidate_id Public_data.empty_student_pegasus).Lift.safe

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.PEGASUS_GENRE;
    Public_data.PEGASUS_CODE_SEXE;
    Public_data.PEGASUS_NOM;
    Public_data.PEGASUS_PRENOM;
    Public_data.PEGASUS_NAISSANCE_DATE;
    Public_data.PEGASUS_NAISSANCE_PAYS;
    Public_data.PEGASUS_NAISSANCE_VILLE;
    Public_data.PEGASUS_NAISSANCE_DEPARTEMENT;
    Public_data.PEGASUS_NATIONALITE;
    Public_data.PEGASUS_DOUBLE_NATIONALITE;
    Public_data.PEGASUS_SS_NUMERO;
    Public_data.PEGASUS_SS_CLE;
    Public_data.PEGASUS_EMAIL_ECOLE_STD;
    Public_data.PEGASUS_EMAIL_PERSONNEL;
    Public_data.PEGASUS_VOIE;
    Public_data.PEGASUS_VOIE_SUITE;
    Public_data.PEGASUS_CODEPOSTAL;
    Public_data.PEGASUS_VILLE;
    Public_data.PEGASUS_PAYS;
    Public_data.PEGASUS_TEL_PORTABLE;
    Public_data.PEGASUS_PRODUIT_CODE;
    Public_data.PEGASUS_PRODUIT_LIBELLE;
    Public_data.PEGASUS_PRODUIT_LIBELLE_SUITE;
    Public_data.PEGASUS_GROUPES;
    Public_data.PEGASUS_OBJECTIF__STATUT;
    Public_data.PEGASUS_LOGIN_STD;
    Public_data.PEGASUS_NUMERO_INE;
    Public_data.PEGASUS_STATUT_INSCRIPTION;
    Public_data.PEGASUS_CLOTURE_DATE;
    Public_data.PEGASUS_CLOTURE_STATUT;
    Public_data.PEGASUS_NUMERO_ETU_ECOLE;
    Public_data.PEGASUS_DOMAINE;
    Public_data.PEGASUS_NOM_DE_L_ETABLISSEMENT;
    Public_data.PEGASUS_REGION;
    Public_data.PEGASUS_TYPE_D_ACTION_FORMATION;
    Public_data.PEGASUS_NIVEAU_DE_FORMATION;
    Public_data.PEGASUS_ID_FORMATION_PERSONNALISEE;
    Public_data.PEGASUS_ID_PRODUIT_DE_FORMATION;
    Public_data.PEGASUS_ID_PERSONNE_A_CONTACTER;
    Public_data.PEGASUS_NUMERO_SESSION;
    Public_data.PEGASUS_NUMERO_DE_COMPTE;
    Public_data.PEGASUS_S_AUTORISATION_UTILISE_IMAGE;
    Public_data.PEGASUS_FINANCIER_NOM;
    Public_data.PEGASUS_FINANCIER_PRENOM;
    Public_data.PEGASUS_FINANCIER_GENRE;
    Public_data.PEGASUS_FINANCIER_SITUATION_FAMILIALE;
    Public_data.PEGASUS_FINANCIER_NOM_DE_LA_VOIE;
    Public_data.PEGASUS_FINANCIER_NOM_DE_LA_VOIE_SUITE;
    Public_data.PEGASUS_FINANCIER_CODE_POSTAL;
    Public_data.PEGASUS_FINANCIER_VILLE;
    Public_data.PEGASUS_FINANCIER_PAYS;
    Public_data.PEGASUS_FINANCIER_TEL_BUREAU;
    Public_data.PEGASUS_FINANCIER_TEL_DOMICILE;
    Public_data.PEGASUS_FINANCIER_PROFESSION;
    Public_data.PEGASUS_FINANCIER_EMAIL;
    Public_data.PEGASUS_PARENT_1_NOM;
    Public_data.PEGASUS_PARENT_1_PRENOM;
    Public_data.PEGASUS_PARENT_1_GENRE;
    Public_data.PEGASUS_PARENT_1_SITUATION_FAMILIALE;
    Public_data.PEGASUS_PARENT_1_NOM_DE_LA_VOIE;
    Public_data.PEGASUS_PARENT_1_NOM_DE_LA_VOIE_SUITE;
    Public_data.PEGASUS_PARENT_1_CODE_POSTAL;
    Public_data.PEGASUS_PARENT_1_VILLE;
    Public_data.PEGASUS_PARENT_1_PAYS;
    Public_data.PEGASUS_PARENT_1_TEL_BUREAU;
    Public_data.PEGASUS_PARENT_1_TEL_DOMICILE;
    Public_data.PEGASUS_PARENT_1_PROFESSION;
    Public_data.PEGASUS_PARENT_1_EMAIL;
    Public_data.PEGASUS_PARENT_2_NOM;
    Public_data.PEGASUS_PARENT_2_PRENOM;
    Public_data.PEGASUS_PARENT_2_GENRE;
    Public_data.PEGASUS_PARENT_2_SITUATION_FAMILIALE;
    Public_data.PEGASUS_PARENT_2_NOM_DE_LA_VOIE;
    Public_data.PEGASUS_PARENT_2_NOM_DE_LA_VOIE_SUITE;
    Public_data.PEGASUS_PARENT_2_CODE_POSTAL;
    Public_data.PEGASUS_PARENT_2_VILLE;
    Public_data.PEGASUS_PARENT_2_PAYS;
    Public_data.PEGASUS_PARENT_2_TEL_BUREAU;
    Public_data.PEGASUS_PARENT_2_TEL_DOMICILE;
    Public_data.PEGASUS_PARENT_2_PROFESSION;
    Public_data.PEGASUS_PARENT_2_EMAIL;
    Public_data.PEGASUS_SOLDE__NB_ECHEANCES;
    Public_data.PEGASUS_FC_ECH3_MONTANT;
    Public_data.PEGASUS_FC_ECH1_DATE;
    Public_data.PEGASUS_FC_ECH1_MONTANT;
    Public_data.PEGASUS_FC_ECH2_DATE;
    Public_data.PEGASUS_FC_ECH2_MONTANT;
    Public_data.PEGASUS_FC_ECH3_DATE;
    Public_data.PEGASUS_MODE_DE_REGLEMENT;
    Public_data.PEGASUS_RI_STATUT_UPLOAD_DOC;
    Public_data.PEGASUS_RI_STATUT_PAIEMENT;
    Public_data.PEGASUS_RI_REMARQUE_1;
    Public_data.PEGASUS_RI_REMARQUE_2;
    Public_data.PEGASUS_RI_ETAT_DOSSIER;
    Public_data.PEGASUS_MODE_DE_REGLEMENT_02;
    Public_data.PEGASUS_TYPE_DE_BOURSE;
    Public_data.PEGASUS_ECHELON_BOURSE;
    Public_data.PEGASUS_CVE_NUMERO;
    Public_data.PEGASUS_CVE_STATUT_ETUDIANT;
    Public_data.PEGASUS_CVE_VALIDE;
    Public_data.PEGASUS_POPULATION_PORTAIL;
    Public_data.PEGASUS_BLOCAGE_ADMINISTRATION;
    Public_data.PEGASUS_AUTORISATION_UTILISE_IMAGE;
    Public_data.PEGASUS_SISE_EXOINS_TYPE_ETUDIANT;
    Public_data.PEGASUS_SISE_EXOINS_TYPE_EXONERATION;
    Public_data.PEGASUS_BAC_SERIE;
    Public_data.PEGASUS_AUTRE_NATIONALITE;
    Public_data.PEGASUS_DATE_INSCRIPTION;
    Public_data.PEGASUS_ENS_NO_INDIVIDU;
    Public_data.PEGASUS_ENS_ETUDIANT_NORMALIEN;
    Public_data.PEGASUS_CODE_CSP_PERE;
    Public_data.PEGASUS_CODE_CSP_MERE;
    Public_data.PEGASUS_BAC_DEPARTEMENT;
    Public_data.PEGASUS_SISE_ANETAB;
    Public_data.PEGASUS_NOM_ETAT_CIVIL;
    Public_data.PEGASUS_PRENOM_ETAT_CIVIL;
    Public_data.PEGASUS_NOM_D_USAGE;
    Public_data.PEGASUS_PRENOM_D_USAGE;
    Public_data.PEGASUS_ENS_ETAB_RATTACHEMENT;
    Public_data.PEGASUS_ENS_ETAB_RATT_CONTACT_NOM;
    Public_data.PEGASUS_ENS_ETAB_RATT_CONTACT_EMAIL;
    Public_data.PEGASUS_ENS_STAGE_SUJET;
    Public_data.PEGASUS_ENS_STAGE_LIEU;
    Public_data.PEGASUS_ENS_STAGE_ENCADRANT;
    Public_data.PEGASUS_ENS_STAGE_RAPPORTEUR_1;
    Public_data.PEGASUS_ENS_STAGE_RAPPORTEUR_2;
    Public_data.PEGASUS_TUTEUR_PEDAGOGIQUE;
    Public_data.PEGASUS_ENS_FONCTIONNAIRE;
    Public_data.PEGASUS_ENS_PROMO;
    Public_data.PEGASUS_ENS_STATUT_NORMALIEN;
    Public_data.PEGASUS_ENS_CONCOURS;
    Public_data.PEGASUS_FP_RI_ETAT_DOSSIER;
    Public_data.PEGASUS_FP_RI_STATUT_PAIEMENT;
    Public_data.PEGASUS_FP_RI_STATUT_UPLOAD_DOC;
    Public_data.PEGASUS_FP_NOMBRE_DE_CRETITS_MAXIMUN;
    Public_data.PEGASUS_GROUPES_CAMPAGNE;

  ]

  let keywords_of_interest =
    [
      Public_data.PEGASUS_NOM;
      Public_data.PEGASUS_PRENOM;
      Public_data.PEGASUS_ENS_PROMO;
      Public_data.PEGASUS_GENRE;
      Public_data.PEGASUS_NAISSANCE_DATE;
      Public_data.PEGASUS_PRODUIT_CODE;
      Public_data.PEGASUS_NUMERO_INE;
      Public_data.PEGASUS_NAISSANCE_VILLE;
      Public_data.PEGASUS_NAISSANCE_PAYS;
      Public_data.PEGASUS_ENS_CONCOURS;
    ]

let mandatory_fields =
      [
        lift_pred (fun a -> a.pegasus_firstname) "the first name of the student";
        lift_pred (fun a -> a.pegasus_lastname) "the last name of the student";
        lift_pred (fun a -> a.pegasus_promotion) "promotion";
(*lift_pred (fun a -> a.pegasus_produit_de_formation) "produit_de_formation";

        lift_pred (fun a -> a.pegasus_birth_city_fr) "city of birth";
        lift_pred (fun a -> a.pegasus_birth_country_fr) "country of birth";
        lift_pred (fun a -> a.pegasus_ine) "ine number";*)
      ]

let all_fields =
    let record_name = "dens candidate" in
        [
        lift_string
          ~keyword:Public_data.PEGASUS_ENS_CONCOURS
          ~set_tmp:(Tools.collect_string
                      (fun pegasus_origin x ->
                            {x with pegasus_origin}))
          ~get_tmp:(fun a -> a.pegasus_origin)
          ~get:(fun a -> a.Public_data.pegasus_origin)
          ~set:(fun pegasus_origin a ->
             {a with Public_data.pegasus_origin})
          ~record_name
          ~field_name:"Entry track of the student"
          ~pos:__POS__ ;
          lift_string
            ~keyword:Public_data.PEGASUS_GENRE
            ~set_tmp:(Tools.collect_string
                        (fun pegasus_gender x ->
                              {x with pegasus_gender}))
            ~get_tmp:(fun a -> a.pegasus_gender)
            ~get:(fun a -> a.Public_data.pegasus_gender)
            ~set:(fun pegasus_gender a ->
               {a with Public_data.pegasus_gender})
            ~record_name
            ~field_name:"gender of the student"
            ~pos:__POS__ ;
            lift_string
              ~keyword:Public_data.PEGASUS_NAISSANCE_DATE
              ~set_tmp:(Tools.collect_string
                          (fun pegasus_birthdate x ->
                                {x with pegasus_birthdate}))
              ~get_tmp:(fun a -> a.pegasus_birthdate)
              ~get:(fun a -> a.Public_data.pegasus_birthdate)
              ~set:(fun pegasus_birthdate a ->
                 {a with Public_data.pegasus_birthdate})
              ~record_name
              ~field_name:"birth date of the student"
              ~pos:__POS__ ;

          lift_string
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


                    ]

        let get_pegasus_administrative_data
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
              ~init_state:empty_candidate_id
              ~empty_elt:Public_data.empty_student_pegasus ~add_elt:Remanent_state.Collector_administrative_status.add
              ~mandatory_fields
              ~all_fields
              ?event_opt
              state
            in
            let state = Remanent_state.close_event_opt event state in
            state
