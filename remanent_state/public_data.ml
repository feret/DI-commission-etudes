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


type main_dpt = DI | DMA | ENS | PHYS | IBENS | ECO | DRI | ARTS | LILA
type universite =
  | PSL | UP | UPC | UPS | SU | UPantheonSorbonne | Upartenaire | UENS | UDiderot | UPSud | UPNord | USPN | UDauphine

module StringUnivMap =
  Map_tools.MakeSimplified
    (
    struct
      module Ord   =
        (
        struct
          type t = string * universite
          let compare = compare

        end
        )

      let simplify (s,univ) =
        Special_char.lowercase
          (Special_char.correct_string_txt
             (String.trim s)), univ
    end
    )

let string_of_dpt x =
  match x with
  | DI -> "informatique"
  | DMA -> "mathématiques"
  | IBENS -> "biologie"
  | PHYS -> "physique"
  | ECO -> "économie"
  | DRI -> "échanges DRI"
  | ARTS -> "arts"
  | LILA -> "littératures et langage"
  | ENS -> ""

let dpt_of_string x =
  let x = Special_char.lowercase (Special_char.correct_string_txt (Special_char.correct_string_utf8 (String.trim x))) in
  match x with
  | "informatique" | "di" -> DI
  | "mathematiques" | "dma" -> DMA
  | "physique" -> PHYS
  | "biologie" -> IBENS
  | "" | "ens" -> ENS
  | "eco" | "economie" -> ECO
  | "echanges dri"
  | "relations internationales" -> DRI
  | "litteratures et langage" -> LILA
  | _ -> DI

let file_suffix_of_univ x =
    match x with
    | UENS -> ""
    | PSL -> "_PSL"
    | UPC -> "_UPC"
    | UP -> "_UP"
    | UPS -> "_UPS"
    | SU -> "_SU"
    | UPSud -> "_P11"
    | UPantheonSorbonne -> "_P1"
    | UDiderot -> "_P7"
    | UPNord -> "_P13"
    | USPN -> "_USPN"
    | UDauphine -> "_P9"
    | Upartenaire -> ""


let string_of_universite x =
    match x with
      | UENS -> "ENS"
      | PSL -> "PSL"
      | UPC -> "UPC"
      | UP -> "UP"
      | UPS -> "UPS"
      | SU -> "SU"
      | UPSud -> "P11"
      | UPantheonSorbonne -> "P1"
      | UDiderot -> "P7"
      | UPNord -> "P13"
      | USPN -> "USPN"
      | UDauphine -> "P9"
      | Upartenaire -> ""

let string_of_universite_long_fr x =
          match x with
            | UENS -> "École normale supérieure"
            | PSL -> "Université PSL"
            | UPC -> "Université Paris Cité"
            | UP -> "Université de Paris"
            | UPS -> "Université Paris-Saclay"
            | SU -> "Sorbonne Université"
            | UPSud -> "Université Paris-Sud"
            | UPantheonSorbonne -> "Université Panthéon-Sorbonne"
            | UDiderot -> "Université Paris Diderot"
            | UPNord -> "Université Paris Nord"
            | USPN -> "Université Sorbonne Paris Nord"
            | UDauphine -> "Université Paris Dauphine"
            | Upartenaire -> "Université partenaire"

            let string_of_universite_long_en x =
                      match x with
                        | UENS -> "École normale supérieure"
                        | PSL -> "PSL University"
                        | UPC -> "Paris City University"
                        | UP -> "University of Paris"
                        | UPS -> "Paris-Saclay University"
                        | SU -> "Sorbonne University"
                        | UPSud -> "Paris-Sud University"
                        | UPantheonSorbonne -> "Panthéon-Sorbonne University"
                        | UDiderot -> "Paris Diderot University"
                        | UPNord -> "Paris-Nord University"
                        | USPN -> "Sorbonne Paris Nord University"
                        | UDauphine -> "Paris Dauphine University"
                        | Upartenaire -> "Partner University"


let univ_of_string x =
  let x = Special_char.lowercase (Special_char.correct_string_txt (Special_char.correct_string_utf8 (String.trim x))) in
  match x with
  | "psl" -> PSL
  | "ens" -> UENS
  | "upc" -> UPC
  | "ups" -> UPS
  | "up" -> UP
  | "su" -> SU
  | "upsud" | "p11" -> UPSud
  | "u-pantheon-sorbonne" | "p1"-> UPantheonSorbonne
  | "p7" | "diderot" -> UDiderot
  | "uspn" -> USPN
  | _ -> Upartenaire


type cloud_client = NextCloudCmd
type pdf_generator = PdfLatex
type file_retriever = WGET
type cloud_synchronization_mode = Daemon | CommandLine
type annee = string

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

type language = French | English
type repartition = Annee_de_validation_du_cours | Annee_obtention_du_diplome


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
    holder_promotion: string option;
    funding_begin: annee option;
    funding_end: annee option;
  }

let empty_scholarship =
  {
    organism="";
    holder_firstname="";
    holder_lastname="";
    holder_promotion=None;
    funding_begin=None;
    funding_end=None;
  }
type course_name_translation =
  {
    year: annee ;
    code: string ;
    name: string option ;
    name_en: string option ;
  }

type course_entry =
  {
    gps_entry:string;
    french_entry:string option;
    english_entry:string option;
  }

let empty_course_entry =
  {
    gps_entry = "";
    french_entry = None;
    english_entry = None;
  }

let empty_course_name_translation =
  {
    year = "" ;
    code = "" ;
    name = None ;
    name_en = None ;
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

let empty_cours_a_ajouter =
  {
    coursaj_nom= "";
    coursaj_prenom="";
    coursaj_code=None;
    coursaj_libelle="";
    coursaj_dpt=None;
    coursaj_level="";
    coursaj_note=None;
    coursaj_ects=0.;
    coursaj_annee=""
  }

type note_a_modifier =
  {
    notetm_nom: string;
    notetm_prenom: string;
    notetm_code:string;
    notetm_note:string option;
    notetm_annee:annee;
    notetm_ects:float option;
  }

let empty_note_a_modifier =
  {
    notetm_nom= "";
    notetm_prenom="";
    notetm_code="";
    notetm_note=None;
    notetm_annee="";
    notetm_ects=None;
  }

type tutorat =
  {
    annee_academique: annee ;
    nom_du_tuteur: string option;
    prenom_du_tuteur: string option;
    genre_du_tuteur: genre option;
    courriel_du_tuteur: string option;
    nom_de_l_etudiant: string;
    prenom_de_l_etudiant: string;
    secondaire: main_dpt option;
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
    secondaire = None;
  }

type cursus =
  {
    cursus_annee_academique: annee ;
    cursus_niveau: string;
    cursus_dpt: main_dpt option;
    cursus_univ: universite option;
    cursus_gps: string option;
    inscription: string option;
    inscription_en: string option;
    entete: string option;
    entete_en: string option;
    pied: string option;
    pied_en: string option;
  }

let empty_cursus =
  {
    cursus_annee_academique = "";
    cursus_niveau = "";
    cursus_dpt = None;
    cursus_gps = None;
    cursus_univ = None;
    inscription = None;
    inscription_en = None;
    entete = None;
    entete_en = None;
    pied = None;
    pied_en = None;
  }

  type inscription =
    {
      inscription_annee_academique: annee ;
      inscription_niveau: string;
      inscription_dpt: main_dpt option;
      inscription_univ: universite option;
      inscription_nom: string;
      inscription_prenom: string;
    }

let empty_inscription =
{
  inscription_annee_academique = "" ;
  inscription_niveau = "" ;
  inscription_dpt = None;
  inscription_univ = None ;
  inscription_nom = "";
  inscription_prenom = "";
}

type dpt =
  {
    dpt_key: main_dpt option ;
    dpt_nom: string ;
    dpt_acronyme: string ;
    dpt_genitif: string ;
    dpt_genitif_en: string;
    dpt_bg_color: Color.color option;
    dpt_font_color: Color.color option;
  }

let empty_dpt =
  {
    dpt_key = None ;
    dpt_nom = "" ;
    dpt_acronyme = "" ;
    dpt_genitif = "" ;
    dpt_genitif_en = "";
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

let empty_decision =
  {
    decision_firstname = "";
    decision_lastname = "";
    decision_annee = "";
    decision_program = "";
    decision_dpt = DI ;
    decision_decision = None;
    decision_decision_en = None;
    decision_mean = None;
    decision_mention = None;
    decision_mention_en = None;
    decision_rank = None;
    decision_effectif = None;
    decision_date = None ;
    decision_date_en = None ;
    decision_commission_name = None;
    decision_commission_name_en = None;
    decision_validated = None;
  }

type admission =
  {
    admission_lastname: string;
    admission_firstname: string;
    admission_annee: string;
    admission_decision: string;
    admission_decision_en: string option;
  }

let empty_admission =
  {
    admission_lastname =  "";
    admission_firstname = "";
    admission_annee = "";
    admission_decision = "";
    admission_decision_en = None;
  }

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

let empty_dispense =
  {
    dispense_firstname = "";
    dispense_lastname = "";
    dispense_annee = "";
    dispense_motif = None ;
    dispense_motif_en = None ;
    dispense_program = "" ;
    dispense_dpt = ""
  }

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
      supplement_ects: float;
      supplement_dens: bool;
  }

  type experience_supplement =
   { activite_code: string ;
     activite_activite: string;
     activite_intitule: string;
  }

  type 'a repartition_diplomes =
    { dens: 'a  ; diplomes_nationaux: 'a}

  let  empty_repartition_diplomes = {dens= []; diplomes_nationaux = []}


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

let all_notes_string =
      ["A";"A+";"A-";"B";"B+";"B-";"C";"C+";"C-";"D";"D+";"D-";"E";"E-";"E+";"P";"a";"a+";"a-";"b";"b+";"b-";"c";"c+";"p"]

  let valide_string f =
    List.mem
      (String.trim f)
      ["A";"A+";"A-";"B";"B+";"B-";"C";"C+";"P";"a";"a+";"a-";"b";"b+";"b-";"c";"c+";"p"]
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
    | DensDEC
    | DensInfo
    | DensMath
    | DensPhys
    | Nes
    | EchErasm
    | Info
    | Mpi
    | Pc
    | PensionnaireEtranger
    | Psi
    | Sis
    | M_MPRI
    | ED386

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
      diplome_commission : bool ;
    }

  type dens =
    {
      dens_main_dpt : main_dpt ;
      dens_firstname : string ;
      dens_lastname : string ;
      dens_promotion : string ;
      dens_total_ects : float ;
      dens_current_year_ects : float ;
      dens_sortant: bool;
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
      dens_activite_autre: experience_supplement list;
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
    mentor_student_dpt: main_dpt;
    mentor_secondary: main_dpt option ;
  }

type keywords =
  | Accord
  | Acronyme
  | Annee_Academique
  | Annee_Debut
  | Annee_en_Cours
  | Annee_Fin
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
  | FirstName
  | FullName
  | Genitif
  | Genre
  | Genre_du_tuteur
  | Grade
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
  | Stages_et_Sejours_a_l_Etranger
  | Statut
  | Tuteur
  | Type_de_Financement
  | Universite
  | Valide
  | Ignore

type remove_non_valided_classes =
  | All
  | All_but_current_academic_year
  | All_but_years of annee list
  | All_but_in_progress
  | All_but_in_progress_in_current_academic_year
  | All_but_in_progress_in_years of annee list

module DptMap =
  Map.Make
    (struct
      type t = main_dpt
      let compare = compare
    end)
module DptOptMap =
Map.Make
  (struct
    type t = main_dpt option
    let compare = compare
  end)
module CodeMap = StringMap
module CodeOptMap =
Map.Make
  (struct
    type t = string option
    let compare = compare
  end)
module PromoMap = StringMap
module FinanceurMap = StringMap
module FirstNameMap = StringMap
module LastNameMap = StringMap
module AcronymMap = StringMap
module ProgramMap = StringMap
module LibelleMap = StringMap
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
module DptOptExtendedMap = Map_tools.Collect(DptOptMap)
module DptExtendedMap = Map_tools.Collect(DptMap)

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



let string_of_origin_opt a =
  match a with
  | None -> ""
  | Some AL -> "CPGE khâgne"
  | Some BCPST -> "CPGE Biologie-Chimie-Physique-Sciences de la terre"
  | Some DensDEC -> "concours universitaire sciences cognitives"
  | Some DensInfo -> "concours universitaire informatique"
  | Some EchErasm -> "Erasmus"
  | Some ED386 -> "ED386"
  | Some Info -> "CPGE Informatique"
  | Some Mpi -> "CPGE Math-Physique-Info"
  | Some Pc  -> "CPGE Physique-Chimie"
  | Some PensionnaireEtranger -> "Pensionnaire Étranger"
  | Some Psi -> "CPGE Physique-Sciences de l'Ingénieur"
  | Some Sis -> "sélection Internationale"
  | Some M_MPRI -> "Master Parisien de recherche en informatique"
  | Some DensMath -> "concours universitaire mathématiques"
  | Some DensPhys -> "concours universitaire de physique"
  | Some Nes -> "concours normalien étudiant Sciences"

let string_of_statut_opt a =
  match a with
  | None -> ""
  | Some Eleve_bis -> "Eleve BIS"
  | Some Eleve -> "Eleve"
  | Some Etudiant -> "Etudiant"
  | Some Ex_boursier_si -> "Ancien boursier - selection internationale"
  | Some Ex_eleve -> "Ancien eleve"
  | Some Ex_eleve_bis -> "Ancien eleve bis"
  | Some Ex_etudiant -> "Ancien etudiant"
  | Some Boursier_si ->
    "Boursier - selection internationale"
  | Some Ex_hors_GPS ->
    "Ex hors GPS"
  | Some Hors_GPS ->
    "Hors GPS"
