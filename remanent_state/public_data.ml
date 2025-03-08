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

      let simplify' s =
        let l = String.split_on_char '-' s in
        let l = List.rev_map String.trim (List.rev l) in
        let s = String.concat "-" l in
        s

      let simplify'' s =
        match s with
          | "thedore" -> "theodore"
          | _ -> s
      let simplify s = simplify'' (simplify' (simplify s))

    end
    )


    module StringSet =
      Map_tools.MakeSetSimplified
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

           let simplify s =
             match simplify s with
              | "bouverot - dupuis" -> "bouverot-dupuis"
              |  x -> x
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

    module StringOptStringOptMap =
      Map_tools.MakeSimplified
        (
        struct
          module Ord   =
            (
            struct
              type t = string option * string option
              let compare = compare

            end
            )

          let simplify (s_opt,s'_opt) =
            Tools.map_opt
              (fun s ->
                 Special_char.lowercase
                   (Special_char.correct_string_txt
                      (String.trim s)))
              s_opt, Tools.map_opt
                (fun s ->
                   Special_char.lowercase
                     (Special_char.correct_string_txt
                        (String.trim s)))
                s'_opt
        end
        )

type main_dpt = DI | DMA | ENS | CHIMIE | GEOSCIENCES | PHYS | IBENS | ECO | DRI | ARTS | LILA | DEC | DSA | DSS | GEOG | HIST | ECLA

type specific = Musicologie | Sciences_Cognitives | Environnement

type mineure = DPT of main_dpt | Specific of specific

type universite =
  | PSL | UP | UPC | UPS | SU | UPantheonSorbonne | Upartenaire | UENS | UDiderot | UPSud | UPNord | USPN | UDauphine

type experience = Recherche | Internationale | Ouverture | Hors_Dens | Transdisciplinaire

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

let string_of_experience x =
  match x with
  | Recherche -> "Recherche"
  | Ouverture -> "Ouverture"
  | Internationale -> "Internationale"
  | Transdisciplinaire -> "Transdisciplinaire"
  | Hors_Dens -> "Hors Dens"

let experience_of_string x =
  let x = Special_char.lowercase (Special_char.correct_string_txt (Special_char.correct_string_utf8 (String.trim x))) in
  match x with
  | "ouverture" -> Ouverture
  | "internationale" | "international" -> Internationale
  | "recherches" | "recherche" -> Recherche
  | "transdisciplinaire" -> Transdisciplinaire
  | "x" | "hors" | "hors dens" -> Hors_Dens
  | _ -> Recherche

let string_of_dpt x =
  match x with
  | DI -> "informatique"
  | DMA -> "mathématiques"
  | CHIMIE -> "chimie"
  | GEOSCIENCES -> "géosciences"
  | DSA -> "sciences de l'antiquité"
  | DSS -> "sciences sociales"
  | IBENS -> "biologie"
  | PHYS -> "physique"
  | ECO -> "économie"
  | DRI -> "échanges DRI"
  | ARTS -> "arts"
  | LILA -> "littératures et langage"
  | DEC -> "études cognitives"
  | GEOG -> "géographie"
  | HIST -> "Histoire"
  | ECLA -> "cultures et langues d'ailleurs"
  | ENS -> ""

let dpt_of_string x =
  match x with
  | "informatique" | "di" -> DI
  | "mathematiques" | "dma" -> DMA
  | "chimie" -> CHIMIE
  | "geosciences" | "gsc"-> GEOSCIENCES
  | "histoire" | "hist" -> HIST
  | "geographie" | "geog" -> GEOG
  | "sciences sociales" | "dss"-> DSS
  | "sciences de l'antiquite" | "dsa" -> DSA
  | "physique" | "phys"-> PHYS
  | "biologie" | "bio" | "ibens"-> IBENS
  | "" | "ens" -> ENS
  | "eco" | "economie" -> ECO
  | "echanges dri"
  | "relations internationales" -> DRI
  | "litteratures et langage" | "lila"-> LILA
  | "etudes cognitives" | "sciences cognitives" | "dec" -> DEC
  | "arts" -> ARTS
  | "cultures et langues d'ailleurs" | "ecla" -> ECLA 
  | _ -> DI

let string_of_specific x =
  match x with
    | Environnement -> "environnement"
    | Musicologie -> "musicologie"
    | Sciences_Cognitives -> "sciences cognitives"

let string_of_mineure x =
    match x with
      | DPT x -> string_of_dpt x
      | Specific x -> string_of_specific x

let mineure_of_string x =
  let x = Special_char.lowercase (Special_char.correct_string_txt (Special_char.correct_string_utf8 (String.trim x))) in
  match x with
    | "ceres" | "environnement" | "environnement et societe"-> Specific Environnement
    | "musicologie" -> Specific Musicologie
    | "sciences cognitives" -> Specific Sciences_Cognitives
    | _ -> DPT (dpt_of_string x)

let dpt_of_string x =
  let x = Special_char.lowercase (Special_char.correct_string_txt (Special_char.correct_string_utf8 (String.trim x))) in
  dpt_of_string x

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
      pegasus_semester:string option;
      pegasus_domain:string option;
  }

let empty_course_pegasus =
{
  pegasus_helisa = "" ;
  pegasus_libelle = "" ;
  pegasus_libelle_en = None ;
  pegasus_prof_nom = None ;
pegasus_prof_prenom = None ;
  pegasus_codegps = None ;
  pegasus_session = "" ;
  pegasus_year = "";
  pegasus_semester = None;
  pegasus_domain = None;
}

type helisa_val = NV | VA | NVJU | VACO | VAJU

let string_of_helisa_val x =
  match x with
    | NV -> "nv"
    | VA -> "va"
    | NVJU -> "nvju"
    | VACO -> "vaco"
    | VAJU -> "vaju"

let get_validation x =
  match String.lowercase_ascii (String.trim x) with
    | "nv" -> Some NV
    | "va" -> Some VA
    | "nvju" -> Some NVJU
    | "vaco" -> Some VACO
    | "vaju" -> Some VAJU
    | _ -> None

type note_pegasus =
   {
    pegasus_note_annee: string ;
    pegasus_note_firstname: string;
    pegasus_note_lastname: string;
    pegasus_note: string option;
    pegasus_validation: helisa_val option;
    pegasus_note_produit: string;
    pegasus_note_code_helisa: string;
}

let empty_note_pegasus =
{
 pegasus_note_annee = "";
 pegasus_note_firstname = "";
 pegasus_note_lastname = "";
 pegasus_note = None;
 pegasus_validation = None;
 pegasus_note_produit = "";
 pegasus_note_code_helisa= "";
}

type stage_pegasus =
    {
      pegasus_stage_firstname: string;
      pegasus_stage_lastname: string;
      pegasus_stage_credits: float option;
      pegasus_stage_valide: bool option;
      pegasus_stage_commentaire: string option;
      pegasus_stage_periode: string option;
      pegasus_stage_sujet: string option;
      pegasus_stage_directeur: string option;
   }

type language = French | English
type repartition = Annee_de_validation_du_cours | Annee_obtention_du_diplome

let empty_stage_pegasus =
  {
    pegasus_stage_firstname = "";
    pegasus_stage_lastname = "";
    pegasus_stage_credits = None;
    pegasus_stage_valide = None;
    pegasus_stage_commentaire = None;
    pegasus_stage_periode = None;
    pegasus_stage_sujet = None;
    pegasus_stage_directeur = None;
  }

let empty_student_id =
  {
    firstname = "";
    lastname = "";
    promotion = None;
  }

let empty_student_pegasus =
      {
        pegasus_firstname = "";
        pegasus_lastname = "";
        pegasus_gender = "";
        pegasus_promotion = "";
        pegasus_origin = "";
        pegasus_birthdate = "";
        pegasus_birth_country_fr = "";
        pegasus_birth_city_fr = "";
        pegasus_produit_de_formation = "";
        pegasus_ine="";
    }

type pedagogical_entry_pegasus =
      {
        pe_firstname: string;
        pe_lastname: string;
        pe_year: string;
        pe_ects: float option;
        pe_libelle: string;
        pe_code_helisa: string;
        pe_code_gps: string option;
        pe_tutor_firstname: string;
        pe_tutor_lastname: string;
        pe_teachers: (string * string) list ;
        pe_student_number: string;
        pe_ine: string;
        pe_semester: string option;
        pe_dens: bool option;
        pe_diploma: string option; 

    }

 module PESET = Set.Make
 (struct
   type t = pedagogical_entry_pegasus 
   let compare = compare
 end) 

let empty_pedagogical_entry =
      {
        pe_firstname = "";
        pe_lastname = "";
        pe_year = "";
        pe_ects = None ;
        pe_libelle = "";
        pe_code_helisa = "";
        pe_code_gps = None;

        pe_tutor_firstname = "";
        pe_tutor_lastname = "";
        pe_student_number = "";
        pe_ine = "";
        pe_teachers = [];
        pe_semester = None;
        pe_dens = None;
        pe_diploma = None; 
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

type cours_a_trier =
  {
    coursat_nom: string;
    coursat_prenom: string;
    coursat_annee: annee;
    coursat_libelle: string;
    coursat_dpt: main_dpt option;
    coursat_codegps: string;
}

let empty_cours_a_trier =
{
  coursat_nom="";
  coursat_prenom="";
  coursat_annee="";
  coursat_libelle="";
  coursat_dpt=None;
  coursat_codegps="";
}

type stage_a_trier =
  {
    stageat_nom: string;
    stageat_prenom: string;
    stageat_annee: annee ;
    stageat_libelle: string;
    stageat_libelle_fr: string;
    stageat_libelle_en: string;
    stageat_activite_fr: string option ;
    stageat_activite_en: string option ;
    stageat_type: experience option ;
}

let empty_stage_a_trier =
{
  stageat_nom = "";
  stageat_prenom ="";
  stageat_annee = "";
  stageat_libelle = "";
  stageat_libelle_fr = "";
  stageat_libelle_en = "";
  stageat_activite_fr = None;
  stageat_activite_en = None;
  stageat_type= None

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
    coursaj_comment:string option;
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
    coursaj_annee="";
    coursaj_comment=None;
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
    label_sad: string option;
    label_sad_en: string option;
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
    label_sad = None;
    label_sad_en = None;
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
         secondary_mineure : mineure ;
         secondary_diplomation_year : string ;
         secondary_accepted : bool option ;
       }

let empty_mineure_majeure ={
  secondary_student_firstname = "" ;
  secondary_student_lastname = "" ;
  secondary_student_promo = "" ;
  secondary_mineure = DPT DI ;
  secondary_diplomation_year = "" ;
  secondary_accepted = None ;
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
     activite_validee: bool option;
  }

  type 'a repartition_diplomes =
    { dens: 'a  ; diplomes_nationaux: 'a}

  let  empty_repartition_diplomes = {dens= []; diplomes_nationaux = []}


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


    let label_of_diplome dip =
        match dip.diplome_cursus.inscription with
          | None ->
            Format.sprintf
              "%s (%s)"
              dip.diplome_niveau
              (string_of_dpt dip.diplome_dpt)
          | Some l -> l

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

  let empty_dens_candidate =
  {
      dens_candidate_main_dpt= DI;
      dens_candidate_firstname = "";
      dens_candidate_lastname = "";
      dens_candidate_promotion = "" ;
      dens_candidate_diplomation_year = "" ;
      dens_candidate_ok = None ;
      dens_candidate_ine = None ;
      dens_candidate_sad = None ;
    }

  type dens =
    {
      dens_main_dpt : main_dpt ;
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
      dens_activite_transdisciplinaire: experience_supplement list;
      dens_activite_autre: experience_supplement list;
      dens_diplomation_year: string;
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
    mentor_student_dpt: main_dpt;
    mentor_secondary: main_dpt option ;
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
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_03
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_04
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_05
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_RDD_06
  | PEGASUS_CO_PRODUIT_ENS_CE_EMAIL_GESTIONNAIRE
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_01
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_02
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_03
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_04
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_05
  | PEGASUS_CO_PRODUIT_ENS_EMAIL_RDD_06
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
  | PEGASUS_CO_ANNEE_COURS_OBL_PHASES_PROS
  | PEGASUS_CO_ANNEE_COURS_OBL_CODES_PRODUITS
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
  | PEGASUS_NV_0 
  | PEGASUS_NV_1 
  | PEGASUS_NV_2 
  | PEGASUS_C1 
  | PEGASUS_C2 
  | PEGASUS_C3 
  | PEGASUS_C4 
  | PEGASUS_C5 
  | PEGASUS_C6 
  | PEGASUS_C7 
  | PEGASUS_C8 
  | PEGASUS_C9 
  | PEGASUS_C10 
  | PEGASUS_Ob_Op 
  | PEGASUS_TYPE_PRODUIT 
  | PEGASUS_DATE_DEBUT 
  | PEGASUS_DATE_FIN 
  | PEGASUS_LIEU_SESSION 
  | PEGASUS_COMPTE 
  | PEGASUS_Stand_
  | PEGASUS_Credit 
  | PEGASUS_Coef 
  | PEGASUS_NIVEAU 
  | PEGASUS_NATURE 
  | PEGASUS_QUANTITE 
  | PEGASUS_UNITE 
  | PEGASUS_TYPE_DE_PRESTATION 
  | PEGASUS_TYPE_DE_CALCUL_DE_MOYENNE 
  | PEGASUS_TYPE_SOMMATION_CREDIT 
  | PEGASUS_NOTE_SEUIL_ACCES_CREDIT 
  | PEGASUS_NOTE_SUR 
  | PEGASUS_TYPE_D_ARRONDI_NOTE 
  | PEGASUS_RESPONSABLE_PEDAGOGIQUE 
  | PEGASUS_ADM_NOM 
  | PEGASUS_ADM_PRENOM 
  | PEGASUS_RESPONSABLE_ADMINISTRATIF 
  | PEGASUS_ENS_CE_EMAIL_RDD_01 
  | PEGASUS_COURS_OBL_CODES_PRODUITS 
  | PEGASUS_COURS_OBL_PHASES_PROS 
  | PEGASUS_MOODLE_ENSEIGNEMENT_FERME 
  | PEGASUS_CHG_VALID 
  | PEGASUS_dh_DUREE_HEURE 
  | PEGASUS_ng_NOMBRE_GROUPES 
  | PEGASUS_ffp_ 
  | PEGASUS_COURS 
  | PEGASUS_TD 
  | PEGASUS_TP 
  | PEGASUS_CM 
  | PEGASUS_ENSEIGNANT_0 
  | PEGASUS_ENSEIGNANT_1 
  | PEGASUS_ENSEIGNANT_2 
  | PEGASUS_ENSEIGNANT_3 
  | PEGASUS_INTITULE 
  | PEGASUS_INTITULE_ANGLAIS 
  | PEGASUS_MOTS_CLES 
  | PEGASUS_MOTS_CLES_ANGLAIS 
  | PEGASUS_OBJECTIFS 
  | PEGASUS_OBJECTIFS_ANGLAIS 
  | PEGASUS_PROGRAMME 
  | PEGASUS_PROGRAMME_ANGLAIS 
  | PEGASUS_PREREQUIS 
  | PEGASUS_PREREQUIS_ANGLAIS 
  | PEGASUS_MODALITE_D_EVAL 
  | PEGASUS_MODALITE_D_EVAL_ANGLAIS 
  | PEGASUS_MODALITE_DIDACT 
  | PEGASUS_MODALITE_DIDACT_ANGLAIS 

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
  module MineureMap =
    Map.Make
      (struct
        type t = mineure
        let compare = compare
      end)
  module MineureOptMap =
  Map.Make
    (struct
      type t = mineure option
      let compare = compare
    end)
module CodeMap = StringMap
module CodeSet = StringSet
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
module LibelleSet = StringSet
module LibelleMap = StringMap
module YearMap =
  Map.Make
    (struct
      type t = annee
      let compare = compare
    end)
module LevelMap = StringMap

module CodeExtendedMap = Map_tools.Collect(CodeMap)
module CodeOptExtendedMap = Map_tools.Collect(CodeOptMap)
module LibelleExtendedMap = Map_tools.Collect(LibelleMap)
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

type cost_member =
  {
    cost_gender: genre;
    cost_firstname: string;
    cost_lastname: string;
    cost_titre_fr: string;
    cost_titre_en: string;
    cost_initials: string;
}

let empty_cost_member =
  {
  cost_gender = Unknown;
  cost_firstname = "";
  cost_lastname = "";
  cost_titre_fr = "";
  cost_titre_en = "";
  cost_initials = "";
}

let string_of_origin_opt a =
  match a with
  | None -> ""
  | Some AL -> "CPGE khâgne"
  | Some BCPST -> "CPGE Biologie-Chimie-Physique-Sciences de la terre"
  | Some DensDEC -> "concours universitaire sciences cognitives"
  | Some DensInfo -> "concours universitaire informatique"
  | Some DensChimie -> "concours universitaire chimie"
  | Some DensGeosciences -> "concours universitaire géosciences"
  | Some EchErasm -> "Erasmus"
  | Some ED386 -> "ED386"
  | Some Info -> "CPGE Informatique"
  | Some Mpi -> "CPGE Math-Physique-Info"
  | Some Pc  -> "CPGE Physique-Chimie"
  | Some PensionnaireEtranger -> "Pensionnaire Étranger"
  | Some Psi -> "CPGE Physique-Sciences de l'Ingénieur"
  | Some Sis -> "sélection Internationale"
  | Some M_MPRI -> "Master Parisien de recherche en informatique"
  | Some DensMath -> "concours universitaire de mathématiques"
  | Some DensBio -> "concours universitaire de biologie"
  | Some DensPhys -> "concours universitaire de physique"
  | Some Nes -> "concours normalien étudiant Sciences"
  | Some Infomp -> "CPGE Informatique (MP)"
  | Some Infompi -> "CPGE Informatique (MPI)Info-MPI"
  | Some Mpimp -> "CPGE Math-Physique-Info / Math-Physique"

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
