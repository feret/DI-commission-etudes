type cloud_client = NextCloudCmd
type pdf_generator = PdfLatex
type file_retriever = WGET
type cloud_synchronization_mode = Daemon | CommandLine
type annee = string

type student_id =
  {
    firstname: string;
    lastname: string;
    promotion: string option;
  }

type scholarship =
  {
    organism: string;
    holder_firstname: string;
    holder_lastname: string;
    holder_promotion: string option
  }


module PromoMap : Map.S with type key = string
module FinanceurMap : Map.S with type key = string
module FirstNameMap : Map.S with type key = string
module LastNameMap : Map.S with type key = string
module YearMap: Map.S with type key = annee

type course =
  {
    year: annee option ;
    code: string option ;
    name: string option ;
    ects: int option ;
    teacher_name: string option ;
    teacher_email: string option ;
  }

type tutorat =
  {
    annee_academique: annee ;
    nom_du_tuteur: string option;
    prenom_du_tuteur: string option;
    courriel_du_tuteur: string option;
    nom_de_l_etudiant: string ;
    prenom_de_l_etudiant: string
  }

type keywords =
  | Accord
  | Annee_Academique
  | Annee_en_Cours
  | Code
  | Commentaire
  | Contact_ENS
  | Contrat
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
  | Grade
  | Inscrit_au_DENS_en
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

type genre =
  | Feminin
  | Masculin

type statut =
  | Eleve
  | Etudiant
