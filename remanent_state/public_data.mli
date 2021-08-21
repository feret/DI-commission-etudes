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

type main_dpt = DI | DMA | ENS | PHYS | IBENS

val string_of_dpt: main_dpt -> string
val dpt_of_string: string -> main_dpt

val empty_student_id: student_id

type scholarship =
  {
    organism: string;
    holder_firstname: string;
    holder_lastname: string;
    holder_promotion: string option
  }

val empty_scholarship: scholarship

module StringMap: Map.S with type key = string
module StringOptMap: Map.S with type key = string option
module DptMap: Map.S with type key = main_dpt
module DptOptMap : Map.S with type key = main_dpt option
module CodeMap : Map.S with type key = string
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

type course =
  {
    year: annee option ;
    code: string option ;
    name: string option ;
    ects: int option ;
    teacher_name: string option ;
    teacher_email: string option ;
  }

val empty_course: course

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
  }

val empty_cours_a_ajouter:cours_a_ajouter

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
    dpt_bg_color: Color.color option;
    dpt_font_color: Color.color option;
  }

val empty_dpt: dpt

type cursus =
  {
    cursus_annee_academique: annee ;
    cursus_niveau: string;
    cursus_dpt: main_dpt option;
    inscription: string option;
    entete: string option;
    pied: string option
  }

val empty_cursus: cursus

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
    decision_mean: float option;
    decision_mention: string option;
    decision_rank: int option;
    decision_effectif: int option;
    decision_date: string option;
    decision_commission_name: string option;
    decision_validated: bool option;
  }

val empty_decision: decision

type admission =
  {
    admission_lastname: string;
    admission_firstname: string;
    admission_annee: string;
    admission_decision: string;
  }

val empty_admission: admission

type dispense =
  {
    dispense_firstname: string;
    dispense_lastname: string;
    dispense_annee: string;
    dispense_motif: string option;
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

type diplome_national =
  {
    diplome_firstname : string ;
    diplome_lastname : string ;
    diplome_gender : genre ;
    diplome_promotion : string ;
    diplome_niveau : string ;
    diplome_dpt : main_dpt ;
    diplome_moyenne : float option;
    diplome_nb_ects : float ;
    diplome_mention : string option;
    diplome_recu : bool ;
    diplome_year : string ;
    diplome_output : string * string ;
  }

type dens =
  {
    dens_firstname : string ;
    dens_lastname : string ;
    dens_promotion : string ;
    dens_total_ects : float ;
    dens_current_year_ects : float ;
    dens_total_potential_ects : float ;
    dens_current_year_potential_ects : float ;
    dens_nb_inscriptions : int ;
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
  | Accord
  | Acronyme
  | Annee_Academique
  | Annee_en_Cours
  | Classement
  | Code
  | Code_gps
  | Commentaire
  | Commission
  | Contact_ENS
  | Contrat
  | Couleur_du_fond
  | Couleur_du_texte
  | Courriel
  | Courriel_du_tuteur
  | Credits
  | Date
  | Date_de_Naissance
  | Decision
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
  | Enseignements
  | Etablissement
  | Etablissement_ou_Entreprise
  | FirstName
  | FullName
  | Genitif
  | Genre
  | Genre_du_tuteur
  | Grade
  | Inscription
  | Inscrit_au_DENS_en
  | Intitule
  | LastName
  | Lettre
  | Libelle
  | Mention
  | Motif
  | Moyenne
  | Niveau
  | Nom_du_tuteur
  | Note
  | Obtenu_en
  | Option
  | Options
  | Origine
  | Organisme_de_Financement
  | Periode
  | Periode_de_Financement
  | Pers_id
  | Pied_de_page
  | Pour_Diplome
  | Prenom_du_tuteur
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
  | Stages_et_Sejours_a_l_Etranger
  | Statut
  | Tuteur
  | Type_de_Financement
  | Valide
  | Ignore

type valide =
  | Bool of bool
  | Abs

type note =
  | Float of float
  | Absent
  | En_cours
  | Abandon
  | Valide_sans_note

type statut =
  | Boursier_si
  | Eleve
  | Etudiant
  | Eleve_bis
  | Ex_boursier_si
  | Ex_eleve
  | Ex_etudiant
  | Hors_GPS
  | Ex_hors_GPS

type origin =
  | DensDEC
  | DensInfo
  | EchErasm
  | Info
  | Mpi
  | Pc
  | PensionnaireEtranger
  | Psi
  | Sis
  | M_MPRI

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
      ('a -> 'a * string) option;
    direction_titre: string;
    direction_departement: string;
  }

type diplome_nat =
  {
    dn_key: string;
    dn_short: string;
    dn_long: string;
    dn_universite: string;
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
