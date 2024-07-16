type cloud_client = NextCloudCmd
type pdf_generator = PdfLatex
type file_retriever = WGET
type cloud_synchronization_mode = Daemon | CommandLine
type annee = string

type language = French | English

type repartition = Annee_de_validation_du_cours | Annee_obtention_du_diplome

type genre =
  | Feminin
  | Masculin
  | Unknown

type student_id =
  {
    firstname: string;
    lastname: string;
    promotion: string option;
  }

type student_pegasus =
  {
    pegasus_firstname: string;
    pegasus_lastname: string;
    pegasus_gender: string;
    pegasus_promotion: string;
    pegasus_origin: string;
    pegasus_birthdate: string;
    pegasus_birth_country_fr: string;
    pegasus_birth_city_fr: string;
    pegasus_produit_de_formation: string;
    pegasus_ine: string;
}

type course_pegasus =
  {
    pegasus_helisa: string ;
    pegasus_libelle: string ;
    pegasus_libelle_en: string option;
    pegasus_prof_nom: string option;
    pegasus_prof_prenom: string option;
    pegasus_codegps: string option;
    pegasus_session:string;
    pegasus_year:string;
    pegasus_semester: string option;
}

type helisa_val = NV | VA | NVJU | VACO | VAJU

val string_of_helisa_val: helisa_val -> string

val get_validation: string -> helisa_val option
type note_pegasus =
   {
    pegasus_note_annee: string ;
    pegasus_note_firstname: string;
    pegasus_note_lastname: string;
    pegasus_note: string option;
    pegasus_validation: helisa_val option;
    pegasus_note_produit: string;
    pegasus_note_code_helisa: string
}

val empty_note_pegasus: note_pegasus
val empty_course_pegasus: course_pegasus
val empty_student_pegasus: student_pegasus

type pedagogical_entry_pegasus =
  {
    pe_firstname: string;
    pe_lastname: string;
    pe_year: string;
    pe_ects: float option ;
    pe_libelle: string;
    pe_code_helisa: string;
    pe_code_gps: string option;
    pe_tutor_firstname: string;
    pe_tutor_lastname: string;
    pe_teachers: (string * string) list ;

    pe_student_number: string;
    pe_ine: string;
    pe_semester: string option;
}

val empty_pedagogical_entry: pedagogical_entry_pegasus

type main_dpt = DI | DMA | ENS | CHIMIE | GEOSCIENCES | PHYS | IBENS | ECO | DRI | ARTS | LILA | DEC
type universite =  | PSL | UP | UPC | UPS | SU | UPantheonSorbonne | Upartenaire | UENS | UDiderot | UPSud | UPNord | USPN | UDauphine
type experience = Recherche | Internationale | Ouverture | Hors_Dens

val string_of_experience: experience -> string
val experience_of_string: string -> experience

val string_of_dpt: main_dpt -> string
val dpt_of_string: string -> main_dpt
val string_of_universite: universite -> string
val string_of_universite_long_fr: universite -> string
val string_of_universite_long_en: universite -> string
val empty_student_id: student_id

type scholarship =
  {
    organism: string;
    holder_firstname: string;
    holder_lastname: string;
    holder_promotion: string option;
    funding_begin: annee option;
    funding_end: annee option;
  }

val empty_scholarship: scholarship

module StringMap: Map.S with type key = string
module StringOptMap: Map.S with type key = string option
module StringOptStringOptMap: Map.S with type key = string option * string option
module StringUnivMap: Map.S with type key = string * universite
module DptMap: Map.S with type key = main_dpt
module DptOptMap : Map.S with type key = main_dpt option
module CodeMap : Map.S with type key = string
module CodeOptMap : Map.S with type key = string option
module CodeSet : Set.S with type elt = string
module PromoMap : Map.S with type key = string
module FinanceurMap : Map.S with type key = string
module FirstNameMap : Map.S with type key = string
module LastNameMap : Map.S with type key = string
module YearMap: Map.S with type key = annee
module AcronymMap: Map.S with type key = string
module ProgramMap: Map.S with type key = string
module LibelleMap: Map.S with type key = string
module LevelMap: Map.S with type key = string

module CodeExtendedMap : Map_tools.Collect
  with type key = CodeMap.key
   and type 'a t = 'a CodeMap.t

module CodeOptExtendedMap : Map_tools.Collect
  with type key = CodeOptMap.key
  and type 'a t = 'a CodeOptMap.t

module LibelleExtendedMap : Map_tools.Collect
    with type key = LibelleMap.key
    and type 'a t = 'a LibelleMap.t

module PromoExtendedMap : Map_tools.Collect
  with type key = PromoMap.key
   and type 'a t = 'a PromoMap.t

module FinanceurExtendedMap : Map_tools.Collect
  with type key = FinanceurMap.key
   and type 'a t = 'a FinanceurMap.t

module FirstNameExtendedMap : Map_tools.Collect
  with type key = FirstNameMap.key
   and type 'a t = 'a FirstNameMap.t

module LastNameExtendedMap : Map_tools.Collect
  with type key = LastNameMap.key
   and type 'a t = 'a LastNameMap.t

module YearExtendedMap: Map_tools.Collect
  with type key = YearMap.key
   and type 'a t = 'a YearMap.t

module AcronymExtendedMap: Map_tools.Collect
  with type key = AcronymMap.key
   and type 'a t = 'a AcronymMap.t

module ProgramExtendedMap: Map_tools.Collect
  with type key = ProgramMap.key
   and type 'a t = 'a ProgramMap.t

module DptOptExtendedMap: Map_tools.Collect
  with type key = DptOptMap.key
   and type 'a t = 'a DptOptMap.t
module DptExtendedMap: Map_tools.Collect
  with type key = DptMap.key
   and type 'a t = 'a DptMap.t
   type cost_member =
     {
       cost_gender: genre;
       cost_firstname: string;
       cost_lastname: string;
       cost_titre_fr: string;
       cost_titre_en: string;
       cost_initials: string;
   }

val empty_cost_member: cost_member


type course_name_translation =
  {
    year: annee ;
    code: string ;
    name: string option ;
    name_en: string option ;
  }

type course_entry =
  {
    gps_entry: string ;
    french_entry: string option;
    english_entry: string option;
  }

val empty_course_entry: course_entry
val empty_course_name_translation: course_name_translation

type course_exception =
  {
    course_exception_year: annee;
    course_exception_code: string;
    course_exception_genre: genre;
    course_exception_firstname: string;
    course_exception_lastname: string;
  }

val empty_course_exception: course_exception

type cours_a_ajouter =
  {
    coursaj_nom: string;
    coursaj_prenom: string;
    coursaj_code:string option;
    coursaj_libelle:string;
    coursaj_dpt:string option;
    coursaj_level:string;
    coursaj_note:float option;
    coursaj_ects:float;
    coursaj_annee:annee;
    coursaj_comment:string option;
  }

val empty_cours_a_ajouter:cours_a_ajouter

type cours_a_trier =
  {
    coursat_nom: string;
    coursat_prenom: string;
    coursat_annee: annee;
    coursat_libelle: string ;
    coursat_dpt: main_dpt option;
    coursat_codegps: string ;
}

val empty_cours_a_trier: cours_a_trier

type stage_a_trier =
  {
    stageat_nom: string;
    stageat_prenom: string;
    stageat_annee: annee ;
    stageat_libelle: string;
    stageat_libelle_fr: string;
    stageat_libelle_en: string;
    stageat_activite_fr: string option;
    stageat_activite_en: string option;
    stageat_type: experience option;

}

val empty_stage_a_trier: stage_a_trier

type note_a_modifier =
  {
    notetm_nom: string;
    notetm_prenom: string;
    notetm_code:string;
    notetm_note:string option;
    notetm_annee:annee;
    notetm_ects:float option;
  }


val empty_note_a_modifier:note_a_modifier

type tutorat =
  {
    annee_academique: annee ;
    nom_du_tuteur: string option;
    prenom_du_tuteur: string option;
    genre_du_tuteur: genre option;
    courriel_du_tuteur: string option;
    nom_de_l_etudiant: string ;
    prenom_de_l_etudiant: string;
    secondaire: main_dpt option;
  }

val empty_tutorat: tutorat

type dpt =
  {
    dpt_key: main_dpt option;
    dpt_nom: string ;
    dpt_acronyme: string ;
    dpt_genitif: string ;
    dpt_genitif_en: string;
    dpt_bg_color: Color.color option;
    dpt_font_color: Color.color option;
  }

val empty_dpt: dpt

type cursus =
  {
    cursus_annee_academique: annee ;
    cursus_niveau: string;
    cursus_dpt: main_dpt option;
    cursus_univ: universite option;
    cursus_gps: string option;
    inscription: string option;
    inscription_en: string option;
    label_sad: string option;
    label_sad_en: string option;
    entete: string option;
    entete_en: string option;
    pied: string option;
    pied_en: string option
  }

val empty_cursus: cursus

type inscription =
  {
    inscription_annee_academique: annee ;
    inscription_niveau: string;
    inscription_dpt: main_dpt option;
    inscription_univ: universite option;
    inscription_nom: string;
    inscription_prenom: string;
  }

val empty_inscription: inscription

type program =
  {
    code_gps: string ;
    dpt_acronym: string option ;
    level: string option ;
    label: string option ;
  }

val empty_program: program

type cursus_exception =
  {
    student_firstname: string;
    student_lastname: string;
    class_dpt: string;
    class_level: string;
    annee_de_validation: string;
    codecours: string;
  }

val empty_cursus_exception: cursus_exception

type compensation =
  {
    comp_firstname: string;
    comp_lastname: string;
    comp_annee: string;
    comp_codecours: string
  }

val empty_compensation: compensation

type decision =
  {
    decision_firstname: string;
    decision_lastname: string;
    decision_annee: string;
    decision_program: string;
    decision_dpt: main_dpt;
    decision_decision: string option;
    decision_decision_en: string option;
    decision_mean: float option;
    decision_mention: string option;
    decision_mention_en: string option;
    decision_rank: int option;
    decision_effectif: int option;
    decision_date: string option;
    decision_date_en: string option;
    decision_commission_name: string option;
    decision_commission_name_en: string option;
    decision_validated: bool option;
  }

val empty_decision: decision

type admission =
  {
    admission_lastname: string;
    admission_firstname: string;
    admission_annee: string;
    admission_decision: string;
    admission_decision_en: string option;
  }

val empty_admission: admission

type dispense =
  {
    dispense_firstname: string;
    dispense_lastname: string;
    dispense_annee: string;
    dispense_motif: string option;
    dispense_motif_en: string option;
    dispense_program: string;
    dispense_dpt: string;
  }

val empty_dispense: dispense

type student =
  {
    student_firstname_report : string ;
    student_lastname_report : string ;
    student_promo_report : string ;
  }

type missing_grade =
  {
    missing_grade_firstname : string ;
    missing_grade_lastname : string ;
    missing_grade_promotion: string ;
    missing_grade_year : string ;
    missing_grade_dpt: string ;
    missing_grade_dpt_indice: string ;
    missing_grade_code_gps: string ;
    missing_grade_teacher: string ;
    missing_grade_intitule: string ;
  }

type missing_mentor =
  {
    missing_mentor_firstname : string ;
    missing_mentor_lastname : string ;
    missing_mentor_promotion : string ;
    missing_mentor_year : string ;
  }

type missing_internship_description =
  {
    missing_internship_firstname : string ;
    missing_internship_lastname : string ;
    missing_internship_promotion : string ;
    missing_internship_year : string ;
    missing_internship_code_gps: string ;
    missing_internship_intitule: string ;
  }

type parcours_universitaire =
{
  etablissement_parcours: string;
  domaine_parcours: string;
  annee_obtention_parcours: string;
}

type cours_supplement =
 {
    supplement_code: string;
    supplement_discipline: string;
    supplement_intitule: string;
    supplement_validation_year: annee;
    supplement_ects: float;
    supplement_dens: bool;
    supplement_extra: bool;
}

type experience_supplement =
{ activite_code: string ;
  activite_activite: string;
  activite_intitule: string;
  activite_activite_fr: string option;
  activite_intitule_fr: string option;
  activite_activite_en: string option ;
  activite_intitule_en: string;
  activite_ects: float ;
  activite_annee: string ;
}

type 'a repartition_diplomes =
  { dens: 'a  ; diplomes_nationaux: 'a}

val empty_repartition_diplomes: cours_supplement list repartition_diplomes

type valide =
  | Bool of bool
  | Abs

type note =
  | Float of float
  | String of string
  | Temporary of float
  | Absent
  | En_cours
  | Abandon
  | Valide_sans_note

val all_notes_string: string list

val valide_string: string -> bool

type statut =
  | Boursier_si
  | Eleve
  | Etudiant
  | Eleve_bis
  | Ex_boursier_si
  | Ex_eleve
  | Ex_eleve_bis
  | Ex_etudiant
  | Hors_GPS
  | Ex_hors_GPS

type origin =
  | AL
  | BCPST
  | DensBio
  | DensChimie
  | DensGeosciences
  | DensDEC
  | DensInfo
  | DensMath
  | DensPhys
  | Nes
  | EchErasm
  | Info
  | Infompi
  | Infomp
  | Mpi
  | Mpimp
  | Pc
  | PensionnaireEtranger
  | Psi
  | Sis
  | M_MPRI
  | ED386

val file_suffix_of_univ: universite -> string
val univ_of_string: string -> universite

type diplome_national =
  {
    diplome_firstname : string ;
    diplome_lastname : string ;
    diplome_origine : origin option ;
    diplome_statut : statut option ;
    diplome_ranking : int option ;
    diplome_effectif : int option ;
    diplome_gender : genre ;
    diplome_promotion : string ;
    diplome_niveau : string ;
    diplome_dpt : main_dpt ;
    diplome_univ_key : universite ;
    diplome_cursus : cursus ;
    diplome_moyenne : float option;
    diplome_nb_ects : float ;
    diplome_mention : string option;
    diplome_recu : bool ;
    diplome_year : string ;
    diplome_commission: bool ;
  }

type dens_candidate =
  {
    dens_candidate_main_dpt: main_dpt ;
    dens_candidate_firstname : string ;
    dens_candidate_lastname : string ;
    dens_candidate_promotion : string ;
    dens_candidate_diplomation_year : string ;
    dens_candidate_ok : bool option ;
    dens_candidate_ine : string option ;
    dens_candidate_sad : int option ;
  }

val empty_dens_candidate: dens_candidate

type dens =
  {
    dens_main_dpt: main_dpt ;
    dens_birthdate : string option ;
    dens_firstname : string ;
    dens_lastname : string ;
    dens_promotion : string ;
    dens_total_ects : float ;
    dens_current_year_ects : float ;
    dens_sortant: bool option;
    dens_derogation: bool;
    dens_total_potential_ects : float ;
    dens_current_year_potential_ects : float ;
    dens_nb_inscriptions : int ;
    dens_nb_mandatory_course : int ;
    dens_nb_math_course : int ;
    dens_nb_math_and_math_info_course : int ;
    dens_master : diplome_national list;
    dens_parcours: diplome_national list ;
    dens_cours_a_trier: cours_supplement list repartition_diplomes ;
    dens_cours_discipline_principale: cours_supplement list repartition_diplomes ;
    dens_cours_hors_disciplines_principale: cours_supplement list repartition_diplomes;
    dens_cours_par_dpt: cours_supplement list repartition_diplomes StringMap.t;
    dens_cours_activite: cours_supplement list;
    dens_cours_langue: cours_supplement list;
    dens_cours_mineure: cours_supplement list repartition_diplomes StringMap.t;
    dens_cours_majeure: cours_supplement list repartition_diplomes StringMap.t;
    dens_activite_a_trier: experience_supplement list;
    dens_activite_recherche: experience_supplement list;
    dens_activite_internationale: experience_supplement list;
    dens_activite_ouverture: experience_supplement list;
    dens_activite_autre: experience_supplement list;
    dens_diplomation_year : string ;
    dens_ine: string option;
    dens_sad: int option;
    dens_ok : bool option ;
  }

type mentor =
  {
    mentor_attribution_year : string ;
    mentor_academic_year : string ;
    mentor_student_firstname : string ;
    mentor_student_lastname : string ;
    mentor_student_gender : genre  ;
    mentor_student_promo : string ;
    mentor_firstname : string ;
    mentor_lastname : string ;
    mentor_gender : genre ;
    mentor_email : string ;
    mentor_student_dpt: main_dpt ;
    mentor_secondary : main_dpt option ;
  }

type keywords =
  | Accepte
  | Accord
  | Acronyme
  | Activite
  | Activite_en
  | Annee_Academique
  | Annee_Debut
  | Annee_en_Cours
  | Annee_Fin
  | Annee_de_Validation_du_Cours
  | Classement
  | Code
  | Code_gps
  | Commentaire
  | Commission
  | Commission_en
  | Contact_ENS
  | Contrat
  | Couleur_du_fond
  | Couleur_du_texte
  | Courriel
  | Courriel_du_tuteur
  | Credits
  | Date
  | Date_en
  | Date_de_Naissance
  | Decision
  | Decision_en
  | Departement
  | Departements
  | Departement_principal
  | Departement_secondaire
  | Derniere_Annee
  | Diplome
  | Diplomes
  | Directeur_de_Stage
  | Directeur_Sujet
  | Discipline_SISE
  | Duree
  | ECTS
  | Effectif
  | Entete
  | Entete_en
  | Entree_GPS
  | Enseignements
  | Etablissement
  | Etablissement_ou_Entreprise
  | Experience
  | FirstName
  | FullName
  | Genitif
  | Genre
  | Genre_du_tuteur
  | Grade
  | Initiales
  | Inscription
  | Inscription_en
  | Inscrit_au_DENS_en
  | Intitule
  | Label
  | LastName
  | Lettre
  | Libelle
  | Mention
  | Mention_en
  | Motif
  | Motif_en
  | Moyenne
  | Name
  | Name_en
  | Niveau
  | Nom_du_tuteur
  | Note
  | Numero_sad
  | Numero_ine
  | Obtenu_en
  | Option
  | Options
  | Origine
  | Organisme_de_Financement
  | Periode
  | Periode_de_Financement
  | Pers_id
  | Pied_de_page
  | Pied_de_page_en
  | Pour_Diplome
  | Prenom_du_tuteur
  | Profil
  | Promo
  | Programme
  | Programme_d_etude
  | Recu
  | Responsable
  | Responsable_local
  | Secondaire
  | Semestre
  | Service_Labo_Dpt
  | Situation
  | Sujet_du_Stage_Type_du_Sejour
  | Sujet_FR
  | Sujet_EN
  | Stages_et_Sejours_a_l_Etranger
  | Statut
  | Titre_FR
  | Titre_EN
  | Tuteur
  | Type_de_Financement
  | Universite
  | Valide
  | Ignore
  | PEGASUS_Code_Produit_Helisa
  | PEGASUS_Libelle
  | PEGASUS_Libelle_Anglais
  | PEGASUS_Type_de_produit
  | PEGASUS_Domaine
  | PEGASUS_Nature_du_produit
  | PEGASUS_Niveau
  | PEGASUS_Nature_de_l_activite
  | PEGASUS_Resp_Adm
  | PEGASUS_Unite
  | PEGASUS_TVA
  | PEGASUS_Prestation
  | PEGASUS_Session
  | PEGASUS_Nb_Maxi_Inscrits
  | PEGASUS_Etat
  | PEGASUS_Lieu
  | PEGASUS_Date_Session
  | PEGASUS_Date_Fin_Session
  | PEGASUS_NB_INSCRITS_ACTIFS
  | PEGASUS_NB_INSCRITS_CLOTURES
  | PEGASUS_PED_NOM
  | PEGASUS_PED_PRENOM
  | PEGASUS_PED_EMAIL
  | PEGASUS_RES_NOM
  | PEGASUS_RES_PRENOM
  | PEGASUS_RES_SOCIETE
  | PEGASUS_SE_DEROULER_DANS_SOCIETE
  | PEGASUS_COMPTE_COMPTABLE
  | PEGASUS_CO_PRODUIT_EDT_COULEUR_DU_PRODUIT
  | PEGASUS_CO_PRODUIT_ID_PAIEMENT_EN_LIGNE
  | PEGASUS_CO_PRODUIT_EMARGER_COURS_MUTUALISE
  | PEGASUS_CO_PRODUIT_ENS_CE_ETAB_NOM
  | PEGASUS_CO_PRODUIT_ENS_CE_ETAB_VILLE
  | PEGASUS_CO_PRODUIT_ENS_CE_ETAB_DEPARTEMENT
  | PEGASUS_CO_PRODUIT_ENS_CE_INTITULE
  | PEGASUS_CO_PRODUIT_ENS_CE_ENSEIGNANT
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_01
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_02
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_GESTIONNAIRE
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_01
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_02
  | PEGASUS_CO_PRODUIT_NOTES
  | PEGASUS_CO_PRODUIT_ID_GIROFLE
  | PEGASUS_CO_PRODUIT_ID_PRODUIT_SF
  | PEGASUS_CO_PRODUIT_ID_PRODUIT_SF_02
  | PEGASUS_CO_PRODUIT_LANGUE_ENSEIGNEMENT
  | PEGASUS_CO_ANNEE_EDT_VISIBLE_SUR_PORTAIL_JUSQUE
  | PEGASUS_CO_ANNEE_EDT_VISIBLE_SUR_WEBPROF_JUSQUE
  | PEGASUS_CO_ANNEE_STATUT_ACCES_DECISIONS_JURY
  | PEGASUS_CO_ANNEE_EMARGER_FORMATION_OUVERTE
  | PEGASUS_CO_ANNEE_EMARGER_ENSEIGNEMENT_FERME
  | PEGASUS_CO_ANNEE_AUTORISATION_EDITION_BULLETIN
  | PEGASUS_CO_ANNEE_AFFICHE_CLASSEMENT_EDITION
  | PEGASUS_GENRE
  | PEGASUS_CODE_SEXE
  | PEGASUS_NOM
  | PEGASUS_PRENOM
  | PEGASUS_NAISSANCE_DATE
  | PEGASUS_NAISSANCE_PAYS
  | PEGASUS_NAISSANCE_VILLE
  | PEGASUS_NAISSANCE_DEPARTEMENT
  | PEGASUS_NATIONALITE
  | PEGASUS_DOUBLE_NATIONALITE
  | PEGASUS_SS_NUMERO
  | PEGASUS_SS_CLE
  | PEGASUS_EMAIL_ECOLE_STD
  | PEGASUS_EMAIL_PERSONNEL
  | PEGASUS_VOIE
  | PEGASUS_VOIE_SUITE
  | PEGASUS_CODEPOSTAL
  | PEGASUS_VILLE
  | PEGASUS_PAYS
  | PEGASUS_TEL_PORTABLE
  | PEGASUS_PRODUIT_CODE
  | PEGASUS_PRODUIT_LIBELLE
  | PEGASUS_PRODUIT_LIBELLE_SUITE
  | PEGASUS_GROUPES
  | PEGASUS_OBJECTIF__STATUT
  | PEGASUS_LOGIN_STD
  | PEGASUS_NUMERO_INE
  | PEGASUS_STATUT_INSCRIPTION
  | PEGASUS_CLOTURE_DATE
  | PEGASUS_CLOTURE_STATUT
  | PEGASUS_NUMERO_ETU_ECOLE
  | PEGASUS_DOMAINE
  | PEGASUS_NOM_DE_L_ETABLISSEMENT
  | PEGASUS_REGION
  | PEGASUS_TYPE_D_ACTION_FORMATION
  | PEGASUS_NIVEAU_DE_FORMATION
  | PEGASUS_ID_FORMATION_PERSONNALISEE
  | PEGASUS_ID_PRODUIT_DE_FORMATION
  | PEGASUS_ID_PERSONNE_A_CONTACTER
  | PEGASUS_NUMERO_SESSION
  | PEGASUS_NUMERO_DE_COMPTE
  | PEGASUS_S_AUTORISATION_UTILISE_IMAGE
  | PEGASUS_FINANCIER_NOM
  | PEGASUS_FINANCIER_PRENOM
  | PEGASUS_FINANCIER_GENRE
  | PEGASUS_FINANCIER_SITUATION_FAMILIALE
  | PEGASUS_FINANCIER_NOM_DE_LA_VOIE
  | PEGASUS_FINANCIER_NOM_DE_LA_VOIE_SUITE
  | PEGASUS_FINANCIER_CODE_POSTAL
  | PEGASUS_FINANCIER_VILLE
  | PEGASUS_FINANCIER_PAYS
  | PEGASUS_FINANCIER_TEL_BUREAU
  | PEGASUS_FINANCIER_TEL_DOMICILE
  | PEGASUS_FINANCIER_PROFESSION
  | PEGASUS_FINANCIER_EMAIL
  | PEGASUS_PARENT_1_NOM
  | PEGASUS_PARENT_1_PRENOM
  | PEGASUS_PARENT_1_GENRE
  | PEGASUS_PARENT_1_SITUATION_FAMILIALE
  | PEGASUS_PARENT_1_NOM_DE_LA_VOIE
  | PEGASUS_PARENT_1_NOM_DE_LA_VOIE_SUITE
  | PEGASUS_PARENT_1_CODE_POSTAL
  | PEGASUS_PARENT_1_VILLE
  | PEGASUS_PARENT_1_PAYS
  | PEGASUS_PARENT_1_TEL_BUREAU
  | PEGASUS_PARENT_1_TEL_DOMICILE
  | PEGASUS_PARENT_1_PROFESSION
  | PEGASUS_PARENT_1_EMAIL
  | PEGASUS_PARENT_2_NOM
  | PEGASUS_PARENT_2_PRENOM
  | PEGASUS_PARENT_2_GENRE
  | PEGASUS_PARENT_2_SITUATION_FAMILIALE
  | PEGASUS_PARENT_2_NOM_DE_LA_VOIE
  | PEGASUS_PARENT_2_NOM_DE_LA_VOIE_SUITE
  | PEGASUS_PARENT_2_CODE_POSTAL
  | PEGASUS_PARENT_2_VILLE
  | PEGASUS_PARENT_2_PAYS
  | PEGASUS_PARENT_2_TEL_BUREAU
  | PEGASUS_PARENT_2_TEL_DOMICILE
  | PEGASUS_PARENT_2_PROFESSION
  | PEGASUS_PARENT_2_EMAIL
  | PEGASUS_SOLDE__NB_ECHEANCES
  | PEGASUS_FC_ECH3_MONTANT
  | PEGASUS_FC_ECH1_DATE
  | PEGASUS_FC_ECH1_MONTANT
  | PEGASUS_FC_ECH2_DATE
  | PEGASUS_FC_ECH2_MONTANT
  | PEGASUS_FC_ECH3_DATE
  | PEGASUS_MODE_DE_REGLEMENT
  | PEGASUS_RI_STATUT_UPLOAD_DOC
  | PEGASUS_RI_STATUT_PAIEMENT
  | PEGASUS_RI_REMARQUE_1
  | PEGASUS_RI_REMARQUE_2
  | PEGASUS_RI_ETAT_DOSSIER
  | PEGASUS_MODE_DE_REGLEMENT_02
  | PEGASUS_TYPE_DE_BOURSE
  | PEGASUS_ECHELON_BOURSE
  | PEGASUS_CVE_NUMERO
  | PEGASUS_CVE_STATUT_ETUDIANT
  | PEGASUS_CVE_VALIDE
  | PEGASUS_POPULATION_PORTAIL
  | PEGASUS_BLOCAGE_ADMINISTRATION
  | PEGASUS_AUTORISATION_UTILISE_IMAGE
  | PEGASUS_SISE_EXOINS_TYPE_ETUDIANT
  | PEGASUS_SISE_EXOINS_TYPE_EXONERATION
  | PEGASUS_BAC_SERIE
  | PEGASUS_AUTRE_NATIONALITE
  | PEGASUS_DATE_INSCRIPTION
  | PEGASUS_ENS_NO_INDIVIDU
  | PEGASUS_ENS_ETUDIANT_NORMALIEN
  | PEGASUS_CODE_CSP_PERE
  | PEGASUS_CODE_CSP_MERE
  | PEGASUS_BAC_DEPARTEMENT
  | PEGASUS_SISE_ANETAB
  | PEGASUS_NOM_ETAT_CIVIL
  | PEGASUS_PRENOM_ETAT_CIVIL
  | PEGASUS_NOM_D_USAGE
  | PEGASUS_PRENOM_D_USAGE
  | PEGASUS_ENS_ETAB_RATTACHEMENT
  | PEGASUS_ENS_ETAB_RATT_CONTACT_NOM
  | PEGASUS_ENS_ETAB_RATT_CONTACT_EMAIL
  | PEGASUS_ENS_STAGE_SUJET
  | PEGASUS_ENS_STAGE_LIEU
  | PEGASUS_ENS_STAGE_ENCADRANT
  | PEGASUS_ENS_STAGE_RAPPORTEUR_1
  | PEGASUS_ENS_STAGE_RAPPORTEUR_2
  | PEGASUS_TUTEUR_PEDAGOGIQUE
  | PEGASUS_ENS_FONCTIONNAIRE
  | PEGASUS_ENS_PROMO
  | PEGASUS_ENS_STATUT_NORMALIEN
  | PEGASUS_ENS_CONCOURS
  | PEGASUS_FP_NOMBRE_DE_CRETITS_MAXIMUN
  | PEGASUS_FP_RI_ETAT_DOSSIER
  | PEGASUS_FP_RI_STATUT_PAIEMENT
  | PEGASUS_FP_RI_STATUT_UPLOAD_DOC
  | PEGASUS_GROUPES_CAMPAGNE

type remove_non_valided_classes =
  | All
  | All_but_current_academic_year
  | All_but_years of annee list
  | All_but_in_progress
  | All_but_in_progress_in_current_academic_year
  | All_but_in_progress_in_years of annee list

type 'a direction_des_etudes =
  {
    direction_initiales: string;
    direction_nom_complet: string;
    direction_genre : genre;
    direction_signature:
      ('a -> 'a * string list) option;
    direction_titre: string;
    direction_departement: string;
  }

type diplome_nat =
  {
    dn_key: string;
    dn_short: string;
    dn_long: string;
    dn_universite: string;
    dn_univ_key: universite;
    dn_niveau: string;
    dn_departement:main_dpt;
  }

type diplome_ens =
  {
    dens_key: string;
    nb_inscription_list: int list;
    dens_short: string;
    which_year_string: string;
  }

type sous_commission =
  | Diplome_National of diplome_nat
  | Diplome_ENS of diplome_ens

type 'a commission =
  {
    commission_signataires: 'a direction_des_etudes list;
    commission_sous_commissions: sous_commission list;
    commission_long_date: string;
    commission_year: annee;
  }

val string_of_origin_opt: origin option -> string
val string_of_statut_opt: statut option -> string

type internship =
  {
    internship_student_firstname : string ;
    internship_student_lastname : string ;
    internship_student_promotion : string ;
    internship_code_gps : string ;
    internship_year : string ;
    internship_intitule_fr : string ;
    internship_intitule_en : string ;
    internship_sujet_fr : string option ;
    internship_sujet_en : string option ;
    internship_universite_fr : string option ;
    internship_universite_en : string option ;
    internship_resp_ens_firstname : string option;
    internship_resp_ens_lastname : string option;
    internship_resp_ens_gender : genre option;
    internship_resp_host_firstname : string option;
    internship_resp_host_lastname : string option;
    internship_resp_host_gender : genre option;
    internship_en_france : bool option ;
    internship_ectc: int option ;
    internship_grade: note option
  }

type mineure_majeure =
  {
    secondary_student_firstname: string ;
    secondary_student_lastname : string ;
    secondary_student_promo : string ;
    secondary_dpt : main_dpt ;
    secondary_diplomation_year : string ;
    secondary_accepted : bool option ;
  }

val empty_mineure_majeure: mineure_majeure
