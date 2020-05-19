type cloud_client = NextCloudCmd
type file_retriever = WGET
type cloud_synchronization_mode = Daemon | CommandLine

type student_id =
  {
    firstname: string;
    lastname: string;
    promotion: string option;
  }

type course =
  {
    year: int option ;
    code: string option ;
    name: string option ;
    ects: int option ;
    teacher_name: string option ;
    teacher_email: string option ;
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
  | Grade
  | Inscrit_au_DENS_en
  | LastName
  | Lettre
  | Libelle
  | Niveau
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

type note =
  | Float of float
  | Absent
  | En_cours
  | Abandon
  | Valide_sans_note
