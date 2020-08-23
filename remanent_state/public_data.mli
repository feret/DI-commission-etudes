type cloud_client = NextCloudCmd
type pdf_generator = PdfLatex
type file_retriever = WGET
type cloud_synchronization_mode = Daemon | CommandLine
type annee = string

type genre =
  | Feminin
  | Masculin

type student_id =
  {
    firstname: string;
    lastname: string;
    promotion: string option;
  }

val empty_student_id: student_id

type scholarship =
  {
    organism: string;
    holder_firstname: string;
    holder_lastname: string;
    holder_promotion: string option
  }

val empty_scholarship: scholarship

module CodeMap : Map.S with type key = string
module PromoMap : Map.S with type key = string
module FinanceurMap : Map.S with type key = string
module FirstNameMap : Map.S with type key = string
module LastNameMap : Map.S with type key = string
module YearMap: Map.S with type key = annee
module AcronymMap: Map.S with type key = string
module ProgramMap: Map.S with type key = string

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

type tutorat =
  {
    annee_academique: annee ;
    nom_du_tuteur: string option;
    prenom_du_tuteur: string option;
    genre_du_tuteur: genre option;
    courriel_du_tuteur: string option;
    nom_de_l_etudiant: string ;
    prenom_de_l_etudiant: string
  }

val empty_tutorat: tutorat

type dpt =
  {
    dpt_nom: string ;
    dpt_acronyme: string ;
    dpt_genitif: string ;
    dpt_bg_color: Color.color option;
    dpt_font_color: Color.color option;
  }

val empty_dpt: dpt

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
    decision_dpt: string;
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
  | Enseignements
  | Etablissement
  | Etablissement_ou_Entreprise
  | FirstName
  | FullName
  | Genitif
  | Genre
  | Genre_du_tuteur
  | Grade
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
  | Pour_Diplome
  | Prenom_du_tuteur
  | Promo
  | Programme
  | Programme_d_etude
  | Recu
  | Responsable
  | Responsable_local
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
  | Ex_eleve
  | Ex_etudiant
  | Hors_GPS

type origin =
  | DensInfo
  | EchErasm
  | Info
  | Mpi
  | Pc
  | PensionnaireEtranger
  | Psi
  | Sis

type remove_non_valided_classes =
  | All
  | All_but_current_academic_year
  | All_but_years of annee list
  | All_but_in_progress
  | All_but_in_progress_in_current_academic_year
  | All_but_in_progress_in_years of annee list
