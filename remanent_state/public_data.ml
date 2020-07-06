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
    dpt_gerundif: string ;
    dpt_bg_color: Color.color option;
    dpt_font_color: Color.color option;
  }

let empty_dpt =
  {
    dpt_nom = "" ;
    dpt_acronyme = "" ;
    dpt_gerundif = "" ;
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

type keywords =
  | Accord
  | Acronyme
  | Annee_Academique
  | Annee_en_Cours
  | Code
  | Code_gps
  | Commentaire
  | Contact_ENS
  | Contrat
  | Couleur_du_fond
  | Couleur_du_texte
  | Courriel
  | Courriel_du_tuteur
  | Credits
  | Date_de_Naissance
  | Departement
  | Departement_principal
  | Departements
  | Departement_secondaire
  | Derniere_Annee
  | Diplome
  | Diplomes
  | Directeur_de_Stage
  | Directeur_Sujet
  | Discipline_SISE
  | Duree
  | ECTS
  | Enseignements
  | Etablissement
  | Etablissement_ou_Entreprise
  | FirstName
  | FullName
  | Genre
  | Genre_du_tuteur
  | Gerondif
  | Grade
  | Inscrit_au_DENS_en
  | Intitule
  | LastName
  | Lettre
  | Libelle
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
  | Eleve
  | Etudiant
  | Eleve_bis

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
module YearMap =
  Map.Make
    (struct
      type t = annee
      let compare = compare
    end)
