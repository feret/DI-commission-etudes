module KeyWordsMap =
  Map.Make
    (struct
      type t = Public_data.keywords
      let compare = compare
    end)

type ('record_tmp) any_field_short =
  { key: Public_data.keywords;
    store:
      Remanent_state.t ->
      string option -> 'record_tmp -> Remanent_state.t * 'record_tmp}

type ('record_tmp,'record) any_field =
  { keyword: Public_data.keywords;
    set_tmp:
      Remanent_state.t ->
      string option -> 'record_tmp -> Remanent_state.t * 'record_tmp ;
    update:
      Remanent_state.t -> 'record_tmp -> 'record -> Remanent_state.t * 'record;
    is_unifyable: Remanent_state.t -> 'record -> 'record -> Remanent_state.t * bool ;
    unify: Remanent_state.t -> 'record -> 'record -> Remanent_state.t * 'record option;
    label_tmp: Remanent_state.t -> 'record_tmp -> Remanent_state.t * string option;
    label1:
      Remanent_state.t ->
      'record -> Remanent_state.t * string option;
    label2:
      Remanent_state.t ->
      'record -> 'record -> Remanent_state.t * string option
  }

let shorten a =
  {key = a.keyword; store = a.set_tmp}

type 'a shared =
  {
    do_at_end_of_file:
      Remanent_state.t -> 'a -> 'a list ->
      Remanent_state.t * 'a list;
    do_at_end_of_array_line:
      Public_data.keywords option list ->
      Remanent_state.t -> 'a -> 'a ->
      'a list ->
      Remanent_state.t * 'a * 'a list;
    do_at_end_of_array:
      Public_data.keywords option list ->
      Remanent_state.t ->
      'a ->
      'a list ->
      Remanent_state.t * 'a * 'a list;
    flush:
      Remanent_state.t -> 'a -> 'a list -> Remanent_state.t * 'a list;
  }

type 'record_tmp specification =
  {
    keywords: Public_data.keywords list;
    of_interest: Public_data.keywords list;
    all_fields:
      'record_tmp any_field_short list;
    default:
      (Remanent_state.t ->
       string option ->
       'record_tmp -> Remanent_state.t * 'record_tmp) ;
    shared_functions: 'record_tmp shared
  }

type 'a preprocessed =
  {
    is_keyword:
      string * int * int * int ->
      Remanent_state.t -> string -> Remanent_state.t * bool;
    action:
      string * int * int * int ->
      Remanent_state.t -> string ->
      Remanent_state.t *
      (Remanent_state.t ->
       string option ->
       'a -> Remanent_state.t * 'a)
        option;
    translate :
      string * int * int * int ->
      Remanent_state.t ->
      string -> Remanent_state.t * Public_data.keywords option;
    flush_required :
      string * int * int * int ->
      Remanent_state.t -> string -> Remanent_state.t * bool option;
    shared: 'a shared
  }

let asso_list =
  [
    Public_data.Accepte, ["accepté"];
    Public_data.Accord, ["accord"];
    Public_data.Acronyme, ["acronyme"];
    Public_data.Activite, ["activité";"activité_fr";"activité(français)"];
    Public_data.Activite_en, ["activity";"activité_en";"acticité(english)";"activité(anglais)"];

    Public_data.Annee_Academique, ["année"; "année académique"];
    Public_data.Annee_Debut, ["année début"];
    Public_data.Annee_Fin, ["année fin"];
    Public_data.Annee_de_Validation_du_Cours, ["année de validation"];
    Public_data.Annee_en_Cours, ["année en cours"];
    Public_data.Classement, ["classement";"rang"];
    Public_data.Code, ["code";"code cours"];
    Public_data.Code_gps, ["code gps"];
    Public_data.Commentaire, ["commentaire"];
    Public_data.Commission, ["commission"];
    Public_data.Commission_en, ["commission_en";"commission(anglais)";"commission(english)"];
    Public_data.Contact_ENS, ["contact";"contact ENS"];
    Public_data.Contrat, ["contrat"];
    Public_data.Couleur_du_fond, ["couleur du fond"];
    Public_data.Couleur_du_texte, ["couleur du texte"];
    Public_data.Courriel, ["courriel"];
    Public_data.Courriel_du_tuteur, ["courriel du tuteur";"email du tuteur"];
    Public_data.Credits, ["crédits"];
    Public_data.Date, ["date"];
    Public_data.Date_en, ["date(english)";"date(anglais)";"date_en"];
    Public_data.Date_de_Naissance, ["date de naissance";"naissance"];
    Public_data.Decision, ["décision"];
    Public_data.Decision_en, ["décision_en";"decision(english)";"decision(anglais)"];
    Public_data.Departement, ["département";"dpt"];
    Public_data.Departements,["département(s)"];
    Public_data.Departement_principal, ["principal";"département principal"];
    Public_data.Departement_secondaire, ["secondaire";"département secondaire"];
    Public_data.Derniere_Annee, ["derniere année"];
    Public_data.Diplome, ["diplôme"];
    Public_data.Diplomes, ["diplôme(s)"];
    Public_data.Directeur_de_Stage, ["directeur de stage"];
    Public_data.Directeur_Sujet, ["directeur -- sujet de recherche"];
    Public_data.Discipline_SISE,["discipline SISE"];
    Public_data.Duree,["durée"];
    Public_data.ECTS,["ECTS*";"ECTS"];
    Public_data.Effectif,["effectif"];
    Public_data.Enseignements,["enseignement(s)"];
    Public_data.Entete,["entete";"en-tete";"en_tete"];
    Public_data.Entete_en,[
                           "headpage";
                           "entete_en";"en-tete_en";"en_tete_en";
                           "entete(anglais)";"en-tete(anglais)";"en_tete(anglais)";
                           "entete(english)";"en-tete(english)";"en_tete(english)";
                          ];
    Public_data.Entree_GPS,["entrée GPS"];
    Public_data.Etablissement,["établissement"];
    Public_data.Etablissement_ou_Entreprise,["établissement ou entreprise"];
    Public_data.Experience,["expérience"];
    Public_data.FirstName, ["prénom";];
    Public_data.FullName, ["nom complet"];
    Public_data.Genitif, ["genitif"];
    Public_data.Genre, ["genre"];
    Public_data.Genre_du_tuteur, ["genre du tuteur"];
    Public_data.Grade, ["grade"];
    Public_data.Inscription, ["inscription"];
    Public_data.Inscription_en, ["inscription_en";"registration";"inscription(anglais)";"inscription(english)"];
    Public_data.Inscrit_au_DENS_en, ["inscrit au DENS en"];
    Public_data.Intitule, ["intitulé"];
    Public_data.Initiales, ["initiales"];
    Public_data.Label, ["label";"libellé(anglais)";"libellé_en"];
    Public_data.LastName, ["nom"];
    Public_data.Lettre, ["lettre"];
    Public_data.Libelle, ["libellé"];
    Public_data.Mention, ["mention"];
    Public_data.Motif, ["motif";"raison"];
    Public_data.Motif_en, ["reason";"raison_en";"motif_en";"motif(english)";"motif(anglais)"];
    Public_data.Moyenne, ["moyenne"];
    Public_data.Name, ["cours"];
    Public_data.Name_en, ["course";"cours_en";"cours(anglais)";"cours(english)"];
    Public_data.Niveau, ["niveau";"level"];
    Public_data.Nom_du_tuteur, ["nom du tuteur"];
    Public_data.Note, ["note"];
    Public_data.Numero_ine, ["ine";"numero ine"];
    Public_data.Numero_sad, ["sad";"numero sad"];
    Public_data.Obtenu_en, ["obtenu en"];
    Public_data.Option, ["option"];
    Public_data.Options, ["options"];
    Public_data.Origine, ["origine";"concours"];
    Public_data.Organisme_de_Financement, ["financement";"financeur";"organisme de financement"];
    Public_data.Periode, ["période"];
    Public_data.Periode_de_Financement, ["période de financement"];
    Public_data.Pers_id, ["pers_id"];
    Public_data.Pied_de_page, ["pied de page";"pied-de-page";"pied_de_page"];
    Public_data.Pied_de_page_en, ["footpage";
                                  "pied de page_en";"pied-de-page_en";"pied_de_page_en";
                                  "pied de page(english)";"pied-de-page(english)";"pied_de_page(english)";
                                  "pied de page(anglais)";"pied-de-page(anglais)";"pied_de_page(anglais)";
                                 ];

    Public_data.Pour_Diplome, ["pour diplôme"];
    Public_data.Prenom_du_tuteur, ["prénom du tuteur"];
    Public_data.Profil,["profil"];
    Public_data.Promo, ["promo";"promotion"];
    Public_data.Programme, ["programme"];
    Public_data.Programme_d_etude,
    ["programme";"programme d'études"; "Pgm études"];
    Public_data.Recu, ["reçu"];
    Public_data.Responsable, ["responsable"];
    Public_data.Responsable_local,
    ["responsable local"];
    Public_data.Secondaire, ["secondaire";"secondary"];
    Public_data.Semestre, ["semestre"];
    Public_data.Service_Labo_Dpt,
    ["service/labo/dpt"];
    Public_data.Situation, ["situation"];
    Public_data.Sujet_du_Stage_Type_du_Sejour,["sujet du stage / Type du séjour";"sujet gps";"sujet(gps)"];
    Public_data.Sujet_FR,["sujet";"sujet(français)"];
    Public_data.Sujet_EN,["topic";"sujet(english)";"sujet(anglais)"];
    Public_data.Stages_et_Sejours_a_l_Etranger,
    ["Stage(s) & Séjour(s) à l'étranger"];
    Public_data.Statut, ["statut"];
    Public_data.Titre_EN, ["titre(anglais)";"titre(english)";"title"];
    Public_data.Titre_FR, ["titre(français)";"titre"];
    Public_data.Tuteur, ["tuteur"];
    Public_data.Type_de_Financement,["type de financement"];
    Public_data.Universite,["université";"university"];
    Public_data.Valide, ["validé"];
    Public_data.Ignore, ["ignore"];
    Public_data.PEGASUS_Code_Produit_Helisa,["code produit helisa"];
    Public_data.PEGASUS_Libelle,["libellé"];
    Public_data.PEGASUS_Libelle_Anglais,["libellé anglais"];
    Public_data.PEGASUS_Type_de_produit,["type de produit"];
    Public_data.PEGASUS_Domaine,["domaine"];
    Public_data.PEGASUS_Nature_du_produit,["nature du produit"];
    Public_data.PEGASUS_Niveau,["niveau"];
    Public_data.PEGASUS_Nature_de_l_activite,["nature de l'activité"];
    Public_data.PEGASUS_Resp_Adm,["resp.adm"];
    Public_data.PEGASUS_Unite,["unité"];
    Public_data.PEGASUS_TVA,["tva"];
    Public_data.PEGASUS_Prestation,["prestation"];
    Public_data.PEGASUS_Session,["session"];
    Public_data.PEGASUS_Nb_Maxi_Inscrits,["nb maxi inscrits"];
    Public_data.PEGASUS_Etat,["etat"];
    Public_data.PEGASUS_Lieu,["lieu"];
    Public_data.PEGASUS_Date_Session,["date session"];
    Public_data.PEGASUS_Date_Fin_Session,["date fin session"];
    Public_data.PEGASUS_NB_INSCRITS_ACTIFS,["nb_inscrits_actifs"];
    Public_data.PEGASUS_NB_INSCRITS_CLOTURES,["nb_inscrits_clotures"];
    Public_data.PEGASUS_PED_NOM,["ped_nom"];
    Public_data.PEGASUS_PED_PRENOM,["ped_prenom"];
    Public_data.PEGASUS_PED_EMAIL,["ped_email"];
    Public_data.PEGASUS_RES_NOM,["res_nom"];
    Public_data.PEGASUS_RES_PRENOM,["res_prenom"];
    Public_data.PEGASUS_RES_SOCIETE,["res_societe"];
    Public_data.PEGASUS_SE_DEROULER_DANS_SOCIETE,["se_derouler_dans_societe"];
    Public_data.PEGASUS_COMPTE_COMPTABLE,["compte_comptable"];
    Public_data.PEGASUS_CO_PRODUIT_EDT_COULEUR_DU_PRODUIT,["co_produit edt couleur du produit"];
    Public_data.PEGASUS_CO_PRODUIT_ID_PAIEMENT_EN_LIGNE,["co_produit id_paiement_en_ligne"];
    Public_data.PEGASUS_CO_PRODUIT_EMARGER_COURS_MUTUALISE,["co_produit emarger.cours_mutualise"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ETAB_NOM,["co_produit ens-ce-etab_nom"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ETAB_VILLE,["co_produit ens-ce-etab_ville"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ETAB_DEPARTEMENT,["co_produit ens-ce-etab_departement"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_INTITULE,["co_produit ens-ce-intitule"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_ENSEIGNANT,["co_produit ens-ce-enseignant"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_01,["co_produit ens-ce-email_rdd_01"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_02,["co_produit ens-ce-email_rdd_02"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_GESTIONNAIRE,["co_produit ens-ce-email_gestionnaire"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_01,["co_produit ens-email_rdd_01"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_02,["co_produit ens-email_rdd_02"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_03,["co_produit ens-email_rdd_03"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_04,["co_produit ens-email_rdd_04"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_05,["co_produit ens-email_rdd_05"];
    Public_data.PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_06,["co_produit ens-email_rdd_06"];
    Public_data.PEGASUS_CO_PRODUIT_ID_GIROFLE,["co_produit id_girofle"];
    Public_data.PEGASUS_CO_PRODUIT_NOTES,["co_produit notes-saisie_directe"];
    Public_data.PEGASUS_CO_PRODUIT_ID_PRODUIT_SF,["co_produit id_produit_sf"];
    Public_data.PEGASUS_CO_PRODUIT_ID_PRODUIT_SF_02,["co_produit id_produit_sf_02"];
    Public_data.PEGASUS_CO_PRODUIT_LANGUE_ENSEIGNEMENT,["co_produit langue enseignement"];
    Public_data.PEGASUS_CO_ANNEE_EDT_VISIBLE_SUR_PORTAIL_JUSQUE,["co_annee edt visible sur portail jusque"];
    Public_data.PEGASUS_CO_ANNEE_EDT_VISIBLE_SUR_WEBPROF_JUSQUE,["co_annee edt visible sur webprof jusque"];
    Public_data.PEGASUS_CO_ANNEE_STATUT_ACCES_DECISIONS_JURY,["co_annee statut acces decisions jury"];
    Public_data.PEGASUS_CO_ANNEE_EMARGER_FORMATION_OUVERTE,["co_annee emarger.formation_ouverte"];
    Public_data.PEGASUS_CO_ANNEE_EMARGER_ENSEIGNEMENT_FERME,["co_annee emarger.enseignement_ferme"];
    Public_data.PEGASUS_CO_ANNEE_AUTORISATION_EDITION_BULLETIN,["co_annee autorisation edition bulletin"];
    Public_data.PEGASUS_CO_ANNEE_AFFICHE_CLASSEMENT_EDITION,["co_annee affiche classement edition"];
    Public_data.PEGASUS_CO_ANNEE_COURS_OBL_PHASES_PROS,["co_annee cours-obl-phases_pros"];
    Public_data.PEGASUS_CO_ANNEE_COURS_OBL_CODES_PRODUITS,["co_annee cours-obl-codes_produits"];
    Public_data.PEGASUS_GENRE,["genre"];
    Public_data.PEGASUS_CODE_SEXE,["code_sexe"];
    Public_data.PEGASUS_NOM,["nom"];
    Public_data.PEGASUS_PRENOM,["prenom"];
    Public_data.PEGASUS_NAISSANCE_DATE,["naissance_date"];
    Public_data.PEGASUS_NAISSANCE_PAYS,["naissance_pays"];
    Public_data.PEGASUS_NAISSANCE_VILLE,["naissance_ville"];
    Public_data.PEGASUS_NAISSANCE_DEPARTEMENT,["naissance_departement"];
    Public_data.PEGASUS_NATIONALITE,["nationalite"];
    Public_data.PEGASUS_DOUBLE_NATIONALITE,["double_nationalite"];
    Public_data.PEGASUS_SS_NUMERO,["ss_numero"];
    Public_data.PEGASUS_SS_CLE,["ss_cle"];
    Public_data.PEGASUS_EMAIL_ECOLE_STD,["email_ecole_std"];
    Public_data.PEGASUS_EMAIL_PERSONNEL,["email_personnel"];
    Public_data.PEGASUS_VOIE,["voie"];
    Public_data.PEGASUS_VOIE_SUITE,["voie_suite"];
    Public_data.PEGASUS_CODEPOSTAL,["codepostal"];
    Public_data.PEGASUS_VILLE,["ville"];
    Public_data.PEGASUS_PAYS,["pays"];
    Public_data.PEGASUS_TEL_PORTABLE,["tel_portable"];
    Public_data.PEGASUS_PRODUIT_CODE,["produit_code"];
    Public_data.PEGASUS_PRODUIT_LIBELLE,["produit_libelle"];
    Public_data.PEGASUS_PRODUIT_LIBELLE_SUITE,["produit_libelle_suite"];
    Public_data.PEGASUS_GROUPES,["groupes"];
    Public_data.PEGASUS_OBJECTIF__STATUT,["objectif__statut"];
    Public_data.PEGASUS_LOGIN_STD,["login_std"];
    Public_data.PEGASUS_NUMERO_INE,["numero_ine"];
    Public_data.PEGASUS_STATUT_INSCRIPTION,["statut_inscription"];
    Public_data.PEGASUS_CLOTURE_DATE,["cloture_date"];
    Public_data.PEGASUS_CLOTURE_STATUT,["cloture_statut"];
    Public_data.PEGASUS_NUMERO_ETU_ECOLE,["numero_etu_ecole"];
    Public_data.PEGASUS_DOMAINE,["domaine"];
    Public_data.PEGASUS_NOM_DE_L_ETABLISSEMENT,["nom_de_l_etablissement"];
    Public_data.PEGASUS_REGION,["region"];
    Public_data.PEGASUS_TYPE_D_ACTION_FORMATION,["type_d_action_formation"];
    Public_data.PEGASUS_NIVEAU_DE_FORMATION,["niveau_de_formation"];
    Public_data.PEGASUS_ID_FORMATION_PERSONNALISEE,["id_formation_personnalisee"];
    Public_data.PEGASUS_ID_PRODUIT_DE_FORMATION,["id_produit_de_formation"];
    Public_data.PEGASUS_ID_PERSONNE_A_CONTACTER,["id_personne_a_contacter"];
    Public_data.PEGASUS_NUMERO_SESSION,["numero_session"];
    Public_data.PEGASUS_NUMERO_DE_COMPTE,["numero_de_compte"];
    Public_data.PEGASUS_S_AUTORISATION_UTILISE_IMAGE,["s_autorisation_utilise_image"];
    Public_data.PEGASUS_FINANCIER_NOM,["financier_nom"];
    Public_data.PEGASUS_FINANCIER_PRENOM,["financier_prenom"];
    Public_data.PEGASUS_FINANCIER_GENRE,["financier_genre"];
    Public_data.PEGASUS_FINANCIER_SITUATION_FAMILIALE,["financier_situation_familiale"];
    Public_data.PEGASUS_FINANCIER_NOM_DE_LA_VOIE,["financier_nom_de_la_voie"];
    Public_data.PEGASUS_FINANCIER_NOM_DE_LA_VOIE_SUITE,["financier_nom_de_la_voie_suite"];
    Public_data.PEGASUS_FINANCIER_CODE_POSTAL,["financier_code_postal"];
    Public_data.PEGASUS_FINANCIER_VILLE,["financier_ville"];
    Public_data.PEGASUS_FINANCIER_PAYS,["financier_pays"];
    Public_data.PEGASUS_FINANCIER_TEL_BUREAU,["financier_tel_bureau"];
    Public_data.PEGASUS_FINANCIER_TEL_DOMICILE,["financier_tel_domicile"];
    Public_data.PEGASUS_FINANCIER_PROFESSION,["financier_profession"];
    Public_data.PEGASUS_FINANCIER_EMAIL,["financier_email"];
    Public_data.PEGASUS_PARENT_1_NOM,["parent_1_nom"];
    Public_data.PEGASUS_PARENT_1_PRENOM,["parent_1_prenom"];
    Public_data.PEGASUS_PARENT_1_GENRE,["parent_1_genre"];
    Public_data.PEGASUS_PARENT_1_SITUATION_FAMILIALE,["parent_1_situation_familiale"];
    Public_data.PEGASUS_PARENT_1_NOM_DE_LA_VOIE,["parent_1_nom_de_la_voie"];
    Public_data.PEGASUS_PARENT_1_NOM_DE_LA_VOIE_SUITE,["parent_1_nom_de_la_voie_suite"];
    Public_data.PEGASUS_PARENT_1_CODE_POSTAL,["parent_1_code_postal"];
    Public_data.PEGASUS_PARENT_1_VILLE,["parent_1_ville"];
    Public_data.PEGASUS_PARENT_1_PAYS,["parent_1_pays"];
    Public_data.PEGASUS_PARENT_1_TEL_BUREAU,["parent_1_tel_bureau"];
    Public_data.PEGASUS_PARENT_1_TEL_DOMICILE,["parent_1_tel_domicile"];
    Public_data.PEGASUS_PARENT_1_PROFESSION,["parent_1_profession"];
    Public_data.PEGASUS_PARENT_1_EMAIL,["parent_1_email"];
    Public_data.PEGASUS_PARENT_2_NOM,["parent_2_nom"];
    Public_data.PEGASUS_PARENT_2_PRENOM,["parent_2_prenom"];
    Public_data.PEGASUS_PARENT_2_GENRE,["parent_2_genre"];
    Public_data.PEGASUS_PARENT_2_SITUATION_FAMILIALE,["parent_2_situation_familiale"];
    Public_data.PEGASUS_PARENT_2_NOM_DE_LA_VOIE,["parent_2_nom_de_la_voie"];
    Public_data.PEGASUS_PARENT_2_NOM_DE_LA_VOIE_SUITE,["parent_2_nom_de_la_voie_suite"];
    Public_data.PEGASUS_PARENT_2_CODE_POSTAL,["parent_2_code_postal"];
    Public_data.PEGASUS_PARENT_2_VILLE,["parent_2_ville"];
    Public_data.PEGASUS_PARENT_2_PAYS,["parent_2_pays"];
    Public_data.PEGASUS_PARENT_2_TEL_BUREAU,["parent_2_tel_bureau"];
    Public_data.PEGASUS_PARENT_2_TEL_DOMICILE,["parent_2_tel_domicile"];
    Public_data.PEGASUS_PARENT_2_PROFESSION,["parent_2_profession"];
    Public_data.PEGASUS_PARENT_2_EMAIL,["parent_2_email"];
    Public_data.PEGASUS_SOLDE__NB_ECHEANCES,["solde__nb_echeances"];
    Public_data.PEGASUS_FC_ECH3_MONTANT,["fc_ech3_montant"];
    Public_data.PEGASUS_FC_ECH1_DATE,["fc_ech1_date"];
    Public_data.PEGASUS_FC_ECH1_MONTANT,["fc_ech1_montant"];
    Public_data.PEGASUS_FC_ECH2_DATE,["fc_ech2_date"];
    Public_data.PEGASUS_FC_ECH2_MONTANT,["fc_ech2_montant"];
    Public_data.PEGASUS_FC_ECH3_DATE,["fc_ech3_date"];
    Public_data.PEGASUS_MODE_DE_REGLEMENT,["mode_de_reglement"];
    Public_data.PEGASUS_RI_STATUT_UPLOAD_DOC,["ri_statut_upload_doc"];
    Public_data.PEGASUS_RI_STATUT_PAIEMENT,["ri_statut_paiement"];
    Public_data.PEGASUS_RI_REMARQUE_1,["ri_remarque_1"];
    Public_data.PEGASUS_RI_REMARQUE_2,["ri_remarque_2"];
    Public_data.PEGASUS_RI_ETAT_DOSSIER,["ri_etat_dossier"];
    Public_data.PEGASUS_MODE_DE_REGLEMENT_02,["mode_de_reglement_02"];
    Public_data.PEGASUS_TYPE_DE_BOURSE,["type_de_bourse"];
    Public_data.PEGASUS_ECHELON_BOURSE,["echelon_bourse"];
    Public_data.PEGASUS_CVE_NUMERO,["cve_numero"];
    Public_data.PEGASUS_CVE_STATUT_ETUDIANT,["cve_statut_etudiant"];
    Public_data.PEGASUS_CVE_VALIDE,["cve_valide"];
    Public_data.PEGASUS_POPULATION_PORTAIL,["population_portail"];
    Public_data.PEGASUS_BLOCAGE_ADMINISTRATION,["blocage_administration"];
    Public_data.PEGASUS_AUTORISATION_UTILISE_IMAGE,["autorisation_utilise_image"];
    Public_data.PEGASUS_SISE_EXOINS_TYPE_ETUDIANT,["sise_exoins_type_etudiant"];
    Public_data.PEGASUS_SISE_EXOINS_TYPE_EXONERATION,["sise_exoins_type_exoneration"];
    Public_data.PEGASUS_BAC_SERIE,["bac_serie"];
    Public_data.PEGASUS_AUTRE_NATIONALITE,["autre_nationalite"];
    Public_data.PEGASUS_DATE_INSCRIPTION,["date_inscription"];
    Public_data.PEGASUS_ENS_NO_INDIVIDU,["ens_no_individu"];
    Public_data.PEGASUS_ENS_ETUDIANT_NORMALIEN,["ens_etudiant_normalien"];
    Public_data.PEGASUS_CODE_CSP_PERE,["code_csp_pere"];
    Public_data.PEGASUS_CODE_CSP_MERE,["code_csp_mere"];
    Public_data.PEGASUS_BAC_DEPARTEMENT,["bac_departement"];
    Public_data.PEGASUS_SISE_ANETAB,["sise_anetab"];
    Public_data.PEGASUS_NOM_ETAT_CIVIL,["nom_etat_civil"];
    Public_data.PEGASUS_PRENOM_ETAT_CIVIL,["prenom_etat_civil"];
    Public_data.PEGASUS_NOM_D_USAGE,["nom_d_usage"];
    Public_data.PEGASUS_PRENOM_D_USAGE,["prenom_d_usage"];
    Public_data.PEGASUS_ENS_ETAB_RATTACHEMENT,["ens_etab_rattachement"];
    Public_data.PEGASUS_ENS_ETAB_RATT_CONTACT_NOM,["ens_etab_ratt_contact_nom"];
    Public_data.PEGASUS_ENS_ETAB_RATT_CONTACT_EMAIL,["ens_etab_ratt_contact_email"];
    Public_data.PEGASUS_ENS_STAGE_SUJET,["ens_stage_sujet"];
    Public_data.PEGASUS_ENS_STAGE_LIEU,["ens_stage_lieu"];
    Public_data.PEGASUS_ENS_STAGE_ENCADRANT,["ens_stage_encadrant"];
    Public_data.PEGASUS_ENS_STAGE_RAPPORTEUR_1,["ens_stage_rapporteur_1"];
    Public_data.PEGASUS_ENS_STAGE_RAPPORTEUR_2,["ens_stage_rapporteur_2"];
    Public_data.PEGASUS_TUTEUR_PEDAGOGIQUE,["tuteur_pedagogique"];
    Public_data.PEGASUS_ENS_FONCTIONNAIRE,["ens_fonctionnaire"];
    Public_data.PEGASUS_ENS_PROMO,["ens_promo"];
    Public_data.PEGASUS_ENS_STATUT_NORMALIEN,["ens_statut_normalien"];
    Public_data.PEGASUS_ENS_CONCOURS,["ens_concours"];
    Public_data.PEGASUS_FP_NOMBRE_DE_CRETITS_MAXIMUN,["fp_nombre de credits maximum"];
    Public_data.PEGASUS_FP_RI_ETAT_DOSSIER,["fp_ri_etat_dossier"];
    Public_data.PEGASUS_FP_RI_STATUT_PAIEMENT,["fp_ri_statut_paiement"];
    Public_data.PEGASUS_FP_RI_STATUT_UPLOAD_DOC,["fp_ri_statut_upload_doc"];
    Public_data.PEGASUS_GROUPES_CAMPAGNE,["groupes_campagne"]

]

(*let flatten l =
  let rec aux l output =
    match l with
    | [] -> output
    | []::tail -> aux tail output
    | (h::t)::tail ->
      aux (t::tail) (h::output)
  in
  aux l []
*)
(*let asso_list =
  List.rev_map
    (fun (a,b) ->
       (a,
        flatten
          (List.rev_map Special_char.expand_string (List.rev b))))
    (List.rev asso_list)*)

let asso_list =
  List.rev_map
    (fun (a,b) ->
       (a,
        List.rev_map Special_char.correct_string (List.rev b)))
    (List.rev asso_list)


let make ?debug state specification =
  let event_opt =
    Some Profiling.Build_keywords_automaton
  in
  let state = Remanent_state.open_event_opt event_opt state
  in
  let function_table =
    Tools.asso_list_map2
      specification.all_fields
      specification.keywords
      (fun x -> x.key)
      (fun x -> x)
      (fun x -> x.key, x.store)
      (fun x -> x,specification.default)
      (fun x _ -> x.key, x.store)
  in
  let function_table =
    Tools.asso_list_map2
      function_table
      specification.of_interest
      fst
      (fun x -> x)
      (fun (x,y) -> (x,(y,false)))
      (fun x -> (x,(specification.default,true)))
      (fun (x,y) _ -> (x,(y,true)))
  in
  let map =
    List.fold_left
      (fun map (a,b) -> KeyWordsMap.add a b map)
      KeyWordsMap.empty
      function_table
  in
  let state, asso_list =
    List.fold_left
      (fun (state, output) (a,list) ->
         match KeyWordsMap.find_opt a map with
         | None ->
           state, output
         | Some data ->
           state,
           List.fold_left
             (fun output elt ->
                (elt,(a,data))::output)
             output
             list)
      (state, [])
      (List.rev asso_list)
  in
  let state, is_keyword, asso =
    Automata.build state
      asso_list
  in
  let is_keyword a b c =
    let state, output = is_keyword a b (Special_char.correct_string c) in
    let state =
      match output, debug with
      | _, (None | Some false) -> state
      | true, _ -> Remanent_state.warn a (Format.sprintf "Keywd: Success:  %s" c) Exit state
      | false, _ ->
        Remanent_state.warn a (Format.sprintf "Keywd: Fail: %s" c) Exit state
    in
  state, output
  in
  let cache = ref (None) in
  let asso pos state x =
    match !cache with
    | Some ((state',x'),output)
      when state'==state && x'==x
      ->
      output
    | _ ->
      let output = asso pos state x in
      let () = cache:=Some ((state,x),output) in
      output
  in
  let gen f state x y =
    let state, asso = asso state x (Special_char.correct_string y) in
    state,
    match asso with
    | None -> None
    | Some asso -> Some (f asso)
  in
  let action = gen (fun x -> fst (snd x)) in
  let action a b c =
    if c = "" then b,Some (fun a _ c -> a,c)
    else action a b c
  in
  let flush_required = gen (fun x -> snd (snd x)) in
  let translate = gen fst in
  let state =
    Remanent_state.close_event_opt event_opt state
  in
  let shared = specification.shared_functions in
  let is_keyword pos state x =
    is_keyword pos state (Special_char.correct_string x)
  in
  state,
  {
    is_keyword;
    translate;
    action;
    flush_required;
    shared;
  }
