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
    decision_mean = None;
    decision_mention = None;
    decision_rank = None;
    decision_effectif = None;
    decision_date = None ;
    decision_commission_name = None;
    decision_validated = None;
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

type remove_non_valided_classes =
  | All
  | All_but_current_academic_year
  | All_but_years of annee list
  | All_but_in_progress
  | All_but_in_progress_in_current_academic_year
  | All_but_in_progress_in_years of annee list

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
