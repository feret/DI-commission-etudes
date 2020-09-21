module StringMap =
  Map_tools.MakeSimplified
    (
    struct
      module Ord   =
        (
        struct
          type t = string
          let compare = compare

        end
        )

      let simplify s =
        Special_char.lowercase
          (Special_char.correct_string_txt
             (String.trim s))
    end
    )

module StringOptMap =
  Map_tools.MakeSimplified
    (
    struct
      module Ord   =
        (
        struct
          type t = string option
          let compare = compare

        end
        )

      let simplify s_opt =
        Tools.map_opt
          (fun s ->
             Special_char.lowercase
               (Special_char.correct_string_txt
                  (String.trim s)))
          s_opt
    end
    )


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

let empty_student_id =
  {
    firstname = "";
    lastname = "";
    promotion = None;
  }

type scholarship =
  {
    organism: string;
    holder_firstname: string;
    holder_lastname: string;
    holder_promotion: string option
  }

let empty_scholarship =
  {
    organism="";
    holder_firstname="";
    holder_lastname="";
    holder_promotion=None;
  }
type course =
  {
    year: annee option ;
    code: string option ;
    name: string option ;
    ects: int option ;
    teacher_name: string option ;
    teacher_email: string option ;
  }

let empty_course =
  {
    year = None ;
    code = None ;
    name = None ;
    ects = None ;
    teacher_name = None ;
    teacher_email = None ;
  }

type course_exception =
  {
    course_exception_year: annee;
    course_exception_code: string;
    course_exception_genre: genre;
    course_exception_firstname: string;
    course_exception_lastname: string;
  }

let empty_course_exception =
  {
    course_exception_year="";
    course_exception_code="";
    course_exception_genre=Masculin;
    course_exception_firstname="";
    course_exception_lastname="";
  }

type tutorat =
  {
    annee_academique: annee ;
    nom_du_tuteur: string option;
    prenom_du_tuteur: string option;
    genre_du_tuteur: genre option;
    courriel_du_tuteur: string option;
    nom_de_l_etudiant: string;
    prenom_de_l_etudiant: string
  }

let empty_tutorat =
  {
    annee_academique = "" ;
    nom_du_tuteur = None ;
    prenom_du_tuteur = None ;
    genre_du_tuteur = None ;
    courriel_du_tuteur = None ;
    nom_de_l_etudiant = "" ;
    prenom_de_l_etudiant = "" ;
  }

type cursus =
  {
    cursus_annee_academique: annee ;
    cursus_niveau: string;
    cursus_dpt: string option;
    inscription: string option;
    entete: string option;
    pied: string option
  }

let empty_cursus =
  {
    cursus_annee_academique = "";
    cursus_niveau = "";
    cursus_dpt = None;
    inscription = None;
    entete = None;
    pied = None;
  }

type dpt =
  {
    dpt_nom: string ;
    dpt_acronyme: string ;
    dpt_genitif: string ;
    dpt_bg_color: Color.color option;
    dpt_font_color: Color.color option;
  }

let empty_dpt =
  {
    dpt_nom = "" ;
    dpt_acronyme = "" ;
    dpt_genitif = "" ;
    dpt_bg_color = None ;
    dpt_font_color = None ;
  }

type program =
  {
    code_gps: string ;
    dpt_acronym: string option ;
    level: string option ;
    label: string option ;
  }

let empty_program =
  {
    code_gps = "" ;
    dpt_acronym = None ;
    level = None ;
    label = None ;
  }

type cursus_exception =
  {
    student_firstname: string;
    student_lastname: string;
    class_dpt: string;
    class_level: string;
    annee_de_validation: string;
    codecours: string;
  }

let empty_cursus_exception =
  {
    student_firstname = "";
    student_lastname = "";
    class_dpt = "";
    class_level = "";
    annee_de_validation = "";
    codecours = "";
  }

type compensation =
  {
    comp_firstname: string;
    comp_lastname: string;
    comp_annee: string;
    comp_codecours: string
  }

let empty_compensation =
  {
    comp_firstname = "";
    comp_lastname = "";
    comp_annee = "";
    comp_codecours = "";
  }

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

let empty_decision =
  {
    decision_firstname = "";
    decision_lastname = "";
    decision_annee = "";
    decision_program = "";
    decision_dpt = "";
    decision_decision = None;
    decision_mean = None;
    decision_mention = None;
    decision_rank = None;
    decision_effectif = None;
    decision_date = None ;
    decision_commission_name = None;
    decision_validated = None;
  }

type admission =
  {
    admission_lastname: string;
    admission_firstname: string;
    admission_annee: string;
    admission_decision: string;
  }

let empty_admission =
  {
    admission_lastname =  "";
    admission_firstname = "";
    admission_annee = "";
    admission_decision = "";
  }

type dispense =
  {
    dispense_firstname: string;
    dispense_lastname: string;
    dispense_annee: string;
    dispense_motif: string option;
    dispense_program: string;
    dispense_dpt: string;
    }

let empty_dispense =
  {
    dispense_firstname = "";
    dispense_lastname = "";
    dispense_annee = "";
    dispense_motif = None ;
    dispense_program = "" ;
    dispense_dpt = ""
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
    diplome_promotion : string ;
    diplome_niveau : string ;
    diplome_dpt : string ;
    diplome_moyenne : float option;
    diplome_nb_ects : float ;
    diplome_mention : string option;
    diplome_recu : bool ;
    diplome_year : string ;
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
    mentor_academic_year : string ;
    mentor_student_firstname : string ;
    mentor_student_lastname : string ;
    mentor_student_gender : genre  ;
    mentor_student_promo : string ;
    mentor_firstname : string ;
    mentor_lastname : string ;
    mentor_gender : genre ;
    mentor_student_dpt: string; 
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

type origin =
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

module DptOptMap = StringOptMap
module CodeMap = StringMap
module PromoMap = StringMap
module FinanceurMap = StringMap
module FirstNameMap = StringMap
module LastNameMap = StringMap
module AcronymMap = StringMap
module ProgramMap = StringMap
module YearMap =
  Map.Make
    (struct
      type t = annee
      let compare = compare
    end)
module LevelMap = StringMap

module CodeExtendedMap = Map_tools.Collect(CodeMap)
module PromoExtendedMap = Map_tools.Collect(PromoMap)
module FinanceurExtendedMap = Map_tools.Collect(FinanceurMap)
module FirstNameExtendedMap = Map_tools.Collect(FirstNameMap)
module LastNameExtendedMap = Map_tools.Collect(LastNameMap)
module AcronymExtendedMap = Map_tools.Collect(AcronymMap)
module ProgramExtendedMap = Map_tools.Collect(ProgramMap)
module YearExtendedMap = Map_tools.Collect(YearMap)
