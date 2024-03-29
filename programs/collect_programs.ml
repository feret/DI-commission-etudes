
type dpt_id  =
  {
    acronyme: string option;
    full_name: string option;
    genitif: string option;
    fontcolor: Color.color option;
    bgcolor: Color.color option;
  }

let empty_dpt =
  {
    acronyme = None;
    full_name = None;
    genitif = None;
    fontcolor = None;
    bgcolor = None;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Acronyme ;
    Public_data.FullName ;
    Public_data.Genitif;
    Public_data.Couleur_du_fond;
    Public_data.Couleur_du_texte;
  ]

let keywords_of_interest =
  [
    Public_data.Acronyme ;
    Public_data.FullName ;
  ]

let event_opt = Some (Profiling.Collect_departement)
let compute_repository = Remanent_state.Collector_departements.get_repository

let lift_pred = Lift.pred_safe
let lift_pred_opt = Lift.pred_opt_safe
let lift_string =
  (Lift.string empty_dpt Public_data.empty_dpt).Lift.safe
let lift_color_opt =
  (Lift.color empty_dpt Public_data.empty_dpt).Lift.opt_safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.full_name) "the name of the department";
    lift_pred (fun a -> a.acronyme) "the acronym of the department";
    lift_pred (fun a -> a.genitif) "the genitive of the department"
  ]

let all_fields =
  let record_name = "departement declaration" in
  [
    lift_string
      ~keyword:Public_data.FullName
      ~set_tmp:(fun state full_name x ->
          state,
          let full_name =
            match full_name with
            | Some x when String.trim x = "" -> None
            | _ -> full_name
          in
          {x with full_name})
      ~get_tmp:(fun a -> a.full_name)
      ~get:(fun a -> a.Public_data.dpt_nom)
      ~set:(fun dpt_nom a -> {a with Public_data.dpt_nom})
      ~field_name:"name of the department"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Acronyme
      ~set_tmp:(fun state acronyme x ->
          state,
          let acronyme =
            match acronyme with
            | Some x when String.trim x = "" -> None
            | _ -> acronyme
          in
          {x with acronyme})
      ~get_tmp:(fun a -> a.acronyme)
      ~get:(fun a -> a.Public_data.dpt_acronyme)
      ~set:(fun dpt_acronyme a -> {a with Public_data.dpt_acronyme})
      ~field_name:"acronym of the department"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Genitif
      ~set_tmp:(fun state genitif x ->
          state,
          let genitif =
            match genitif with
            | Some x when String.trim x = "" -> None
            | _ -> genitif
          in
          {x with genitif})
      ~get_tmp:(fun a -> a.genitif)
      ~get:(fun a -> a.Public_data.dpt_genitif)
      ~set:(fun dpt_genitif a -> {a with Public_data.dpt_genitif})
      ~field_name:"genitif of the department"
      ~record_name
      ~pos:__POS__ ;
    lift_color_opt
      ~keyword:Public_data.Couleur_du_fond
      ~set_tmp:(fun state bgcolor x ->
       let state, bgcolor =
         match bgcolor with
         | Some x when String.trim x = "" -> state, None
         | Some x ->
           begin
             match
               Color.color_of_string
                 (Special_char.lowercase
                    (Special_char.correct_string
                       (String.trim x)))
             with
             | Some x -> state, Some x
             | None ->
               let msg =
                 Format.sprintf
                   "Invalid color (%s)"
                   x
               in
               Remanent_state.warn_dft
                 __POS__
                 msg
                 Exit
                 None
                 state
           end
         | None -> state, None
       in
       state, {x with bgcolor})

      ~get_tmp:(fun a -> a.bgcolor)
      ~get:(fun a -> a.Public_data.dpt_bg_color)
      ~set:(fun dpt_bg_color a -> {a with Public_data.dpt_bg_color})
      ~field_name:"background color"
      ~record_name
      ~pos:__POS__ ;
    lift_color_opt
      ~keyword:Public_data.Couleur_du_texte
      ~set_tmp:(fun state fontcolor x ->
          let state, fontcolor =
            match fontcolor with
            | Some x when String.trim x = "" -> state, None
            | Some x ->
              begin
                match
                  Color.color_of_string
                    (Special_char.lowercase
                       (Special_char.correct_string
                          (String.trim x)))
                with
                | Some x -> state, Some x
                | None ->
                  let msg =
                    Format.sprintf
                      "Invalid color (%s)"
                      x
                  in
                  Remanent_state.warn_dft
                    __POS__
                    msg
                    Exit
                    None
                    state
              end
            | None -> state, None
          in
          state, {x with fontcolor})
      ~get_tmp:(fun a -> a.fontcolor)
      ~get:(fun a -> a.Public_data.dpt_font_color)
      ~set:(fun dpt_font_color a -> {a with Public_data.dpt_font_color})
      ~field_name:"font color"
      ~record_name
      ~pos:__POS__ ;
    ]

let get_dpt
    ?repository
    ?prefix
    ?file_name
    state
  =
  Scan_csv_files.collect_gen
    ~strict:true 
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_dpt
    ~empty_elt:Public_data.empty_dpt
    ~add_elt:Remanent_state.Collector_departements.add
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state

type program_id  =
  {
    dpt_acronym: string option;
    code_gps: string option;
    level: string option;
    intitule: string option;
  }

let empty_program =
  {
    dpt_acronym = None;
    code_gps = None;
    level = None;
    intitule = None;
  }

let lift_string =
  (Lift.string empty_program Public_data.empty_program).Lift.safe
let lift_string_opt =
  (Lift.string empty_program Public_data.empty_program).Lift.opt_safe

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Departement ;
    Public_data.Code_gps;
    Public_data.Niveau ;
    Public_data.Intitule;
  ]

let keywords_of_interest =
  [
    Public_data.Departement ;
    Public_data.Code_gps;
  ]

let event_opt = Some (Profiling.Collect_program)
let compute_repository = Remanent_state.get_programs_list_repository

let mandatory_fields =
  [
    lift_pred (fun a -> a.code_gps)
    "Code gps of academic program is missing";
  ]

let all_fields =
  let record_name = "academic program" in
  [
    lift_string
      ~keyword:Public_data.Code_gps
      ~set_tmp:(fun state code_gps x ->
          state,
          let code_gps =
            match code_gps with
            | Some x when String.trim x = "" -> None
            | _ -> code_gps
          in
          {x with code_gps})
      ~get_tmp:(fun a -> a.code_gps)
      ~get:(fun a -> a.Public_data.code_gps)
      ~set:(fun code_gps a -> {a with Public_data.code_gps})
      ~field_name:"GPS code"
      ~record_name
      ~pos:__POS__;
  lift_string_opt
    ~keyword:Public_data.Departement
    ~set_tmp:(fun state dpt_acronym x ->
        state,
        let dpt_acronym =
          match dpt_acronym with
          | Some x when String.trim x = "" -> None
          | _ -> dpt_acronym
        in
        {x with dpt_acronym})
    ~get_tmp:(fun a -> a.dpt_acronym)
    ~get:(fun a -> a.Public_data.dpt_acronym)
    ~set:(fun dpt_acronym a -> {a with Public_data.dpt_acronym})
    ~field_name:"acronym of the department"
    ~record_name
    ~pos:__POS__;
  lift_string_opt
    ~keyword:Public_data.Intitule
    ~set_tmp:(fun state intitule x ->
        state,
        let intitule =
          match intitule with
          | Some x when String.trim x = "" -> None
          | _ -> intitule
        in
        {x with intitule})
    ~get_tmp:(fun a -> a.intitule)
    ~get:(fun a -> a.Public_data.label)
    ~set:(fun label a -> {a with Public_data.label})
    ~field_name:"label of the program"
    ~record_name
    ~pos:__POS__;

    lift_string_opt
      ~keyword:Public_data.Niveau
      ~set_tmp:(fun state level x ->
          state,
          let level =
            match level with
            | Some x when String.trim x = "" -> None
            | _ -> level
          in
          {x with level})
      ~get_tmp:(fun a -> a.level)
      ~get:(fun a -> a.Public_data.level)
      ~set:(fun level a -> {a with Public_data.level})      ~field_name:"niveau"
      ~record_name
      ~pos:__POS__
  ]

let get_programs
    ?repository
    ?prefix
    ?file_name
    state
  =
  Scan_csv_files.collect_gen
    ~strict:true
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_program
    ~empty_elt:Public_data.empty_program
    ~add_elt:Remanent_state.add_program
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state

type cursus_id  =
  {
    cursus_dpt_acronym: Public_data.main_dpt option;
    cursus_annee: string option;
    cursus_niveau: string option;
    cursus_universite: Public_data.universite option;
    cursus_gps: string option;
    headpage: string option;
    headpage_en: string option;
    label_sad: string option;
    label_sad_en: string option;
    footpage: string option;
    footpage_en: string option;
    inscription: string option;
    inscription_en: string option;
  }

let empty_cursus =
  {
    cursus_dpt_acronym = None;
    cursus_annee = None;
    cursus_niveau = None;
    cursus_universite = None;
    cursus_gps = None;
    headpage = None;
    headpage_en = None;
    label_sad = None;
    label_sad_en = None;
    footpage = None;
    footpage_en = None;
    inscription = None;
    inscription_en = None;
  }

let lift_string =
  (Lift.string empty_cursus Public_data.empty_cursus).Lift.safe
let lift_string_opt =
  (Lift.string empty_cursus Public_data.empty_cursus).Lift.opt_safe
let lift_dpt_opt =
  (Lift.main_dpt empty_cursus Public_data.empty_cursus).Lift.opt_safe
let lift_universite_opt =
  (Lift.universite empty_cursus Public_data.empty_cursus).Lift.opt_safe



let keywords_of_interest =
  [
    Public_data.Departement ;
    Public_data.Annee_Academique;
    Public_data.Niveau;
  ]

let event_opt = Some (Profiling.Collect_cursus)
let compute_repository =
  Remanent_state.get_cursus_list_repository

let mandatory_fields =
  [
    lift_pred (fun a -> a.cursus_niveau)
      "Name of academic cursus is missing";
    lift_pred (fun a -> a.cursus_annee)
      "Year of academic cursus is missing";
    lift_pred_opt (fun a -> a.cursus_dpt_acronym)
      "Dpt of academic cursus is missing";
    lift_pred_opt (fun a -> a.cursus_universite)
      "University of academic cursus is missing";
  ]

let all_fields =
  let record_name = "academic cursus" in
  [
    lift_string
      ~keyword:Public_data.Niveau
      ~set_tmp:(fun state cursus_niveau x ->
          state,
          let cursus_niveau =
            match cursus_niveau with
            | Some x when String.trim x = "" -> None
            | _ -> cursus_niveau
          in
          {x with cursus_niveau})
      ~get_tmp:(fun a -> a.cursus_niveau)
      ~get:(fun a -> a.Public_data.cursus_niveau)
      ~set:(fun cursus_niveau a -> {a with Public_data.cursus_niveau})
      ~field_name:"level"
      ~record_name
      ~pos:__POS__;
    lift_dpt_opt
      ~keyword:Public_data.Departement
      ~set_tmp:(fun state dpt x ->
          state,
          let cursus_dpt_acronym = Tools.map_opt Public_data.dpt_of_string dpt
          in
          {x with cursus_dpt_acronym})
      ~get_tmp:(fun a -> a.cursus_dpt_acronym)
      ~get:(fun a -> a.Public_data.cursus_dpt)
      ~set:(fun cursus_dpt a ->
               {a with Public_data.cursus_dpt})
      ~field_name:"acronym of the department"
      ~record_name
      ~pos:__POS__;
    lift_universite_opt
        ~keyword:Public_data.Universite
        ~set_tmp:(fun state univ x ->
            state,
            let cursus_universite = Tools.map_opt Public_data.univ_of_string univ
            in
            {x with cursus_universite})
        ~get_tmp:(fun a -> a.cursus_universite)
        ~get:(fun a -> a.Public_data.cursus_univ)
        ~set:(fun cursus_univ a ->
                 {a with Public_data.cursus_univ})
        ~field_name:"university"
        ~record_name
        ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(fun state cursus_annee x ->
          state,
          let cursus_annee =
            match cursus_annee with
            | Some x when String.trim x = "" -> None
            | _ -> cursus_annee
          in
          {x with cursus_annee})
      ~get_tmp:(fun a -> a.cursus_annee)
      ~get:(fun a -> a.Public_data.cursus_annee_academique)
      ~set:(fun cursus_annee_academique a ->
          {a with Public_data.cursus_annee_academique})
      ~field_name:"year of the cursus"
      ~record_name
      ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Code_gps
      ~set_tmp:(fun state cursus_gps x ->
            state,
            {x with cursus_gps})
      ~get_tmp:(fun a -> a.cursus_gps)
      ~get:(fun a -> a.Public_data.cursus_gps)
        ~set:(fun cursus_gps a ->
            {a with Public_data.cursus_gps})
        ~field_name:"gps code of the cursus"
        ~record_name
        ~pos:__POS__;
    lift_string_opt
        ~keyword:Public_data.Inscription
        ~set_tmp:(fun state inscription x ->
            state,
            let inscription =
              match inscription with
              | Some x when String.trim x = "" -> None
              | _ -> inscription
            in
              {x with inscription})
        ~get_tmp:(fun a -> a.inscription)
        ~get:(fun a -> a.Public_data.inscription)
        ~set:(fun inscription a ->
            {a with Public_data.inscription})
        ~field_name:"incription notice"
        ~record_name
        ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Inscription_en
      ~set_tmp:(fun state inscription_en x ->
          state,
          let inscription_en =
            match inscription_en with
            | Some x when String.trim x = "" -> None
            | _ -> inscription_en
          in
          {x with inscription_en})
      ~get_tmp:(fun a -> a.inscription_en)
      ~get:(fun a -> a.Public_data.inscription_en)
      ~set:(fun inscription_en a ->
          {a with Public_data.inscription_en})
      ~field_name:"incription notice (anglais)"
      ~record_name
      ~pos:__POS__;
      lift_string_opt
          ~keyword:Public_data.Libelle
          ~set_tmp:(fun state label_sad x ->
              state,
              let label_sad =
                match label_sad with
                | Some x when String.trim x = "" -> None
                | _ -> label_sad
              in
                {x with label_sad})
          ~get_tmp:(fun a -> a.label_sad)
          ~get:(fun a -> a.Public_data.label_sad)
          ~set:(fun label_sad a ->
              {a with Public_data.label_sad})
          ~field_name:"incription notice"
          ~record_name
          ~pos:__POS__;
      lift_string_opt
        ~keyword:Public_data.Label
        ~set_tmp:(fun state label_sad_en x ->
            state,
            let label_sad_en =
              match label_sad_en with
              | Some x when String.trim x = "" -> None
              | _ -> label_sad_en
            in
            {x with label_sad_en})
        ~get_tmp:(fun a -> a.label_sad_en)
        ~get:(fun a -> a.Public_data.label_sad_en)
        ~set:(fun label_sad_en a ->
            {a with Public_data.label_sad_en})
        ~field_name:"incription notice (anglais)"
        ~record_name
        ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Entete
          ~set_tmp:(fun state headpage x ->
              state,
              let headpage =
                match headpage with
                | Some x when String.trim x = "" -> None
                | _ -> headpage
              in
              {x with headpage})
          ~get_tmp:(fun a -> a.headpage)
          ~get:(fun a -> a.Public_data.entete)
          ~set:(fun entete a -> {a with Public_data.entete})
          ~field_name:"headpage"
          ~record_name
          ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Entete_en
      ~set_tmp:(fun state headpage_en x ->
                    state,
                    let headpage_en =
                      match headpage_en with
                      | Some x when String.trim x = "" -> None
                      | _ -> headpage_en
                    in
                    {x with headpage_en})
      ~get_tmp:(fun a -> a.headpage_en)
      ~get:(fun a -> a.Public_data.entete_en)
      ~set:(fun entete_en a -> {a with Public_data.entete_en})
      ~field_name:"headpage (anglais)"
      ~record_name
      ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Pied_de_page
      ~set_tmp:(fun state footpage x ->
          state,
          let footpage =
            match footpage with
            | Some x when String.trim x = "" -> None
            | _ -> footpage
          in
          {x with footpage})
      ~get_tmp:(fun a -> a.footpage)
      ~get:(fun a -> a.Public_data.pied)
      ~set:(fun pied a ->
          {a with Public_data.pied})
      ~field_name:"footpage"
      ~record_name
      ~pos:__POS__;
    lift_string_opt
      ~keyword:Public_data.Pied_de_page_en
      ~set_tmp:(fun state footpage_en x ->
          state,
          let footpage_en =
              match footpage_en with
              | Some x when String.trim x = "" -> None
              | _ -> footpage_en
            in
            {x with footpage_en})
        ~get_tmp:(fun a -> a.footpage_en)
        ~get:(fun a -> a.Public_data.pied_en)
        ~set:(fun pied_en a ->
            {a with Public_data.pied_en})
        ~field_name:"footpage (anglais)"
        ~record_name
        ~pos:__POS__;

  ]

  let keywords_list =
    [
      Public_data.Ignore ;
      Public_data.Departement ;
      Public_data.Annee_Academique;
      Public_data.Niveau;
      Public_data.Universite;
      Public_data.Code_gps;
      Public_data.Inscription;
      Public_data.Inscription_en;
      Public_data.Label;
      Public_data.Libelle;
      Public_data.Entete;
      Public_data.Entete_en;
      Public_data.Pied_de_page;
      Public_data.Pied_de_page_en;
    ]

let get_cursus
    ?repository
    ?prefix
    ?file_name
    state
  =
  Scan_csv_files.collect_gen
    ~strict:true
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_cursus
    ~empty_elt:Public_data.empty_cursus
    ~add_elt:Remanent_state.add_cursus
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state

type exception_id  =
  {
    dpt: string option;
    code_cours: string option;
    course_level: string option;
    student_lastname: string option;
    student_firstname: string option;
    annee_de_validation: string option
  }

let empty_exception =
  {
    dpt = None ;
    code_cours = None ;
    course_level = None ;
    student_lastname = None ;
    student_firstname = None ;
    annee_de_validation = None ;
  }


let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.Departement ;
    Public_data.Code_gps;
    Public_data.Niveau;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
  ]

let keywords_of_interest =
  [
    Public_data.Departement ;
    Public_data.Code_gps;
    Public_data.Niveau;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
  ]

let event_opt = Some (Profiling.Collect_cursus_exceptions)
let compute_repository =
  Remanent_state.get_cursus_exceptions_list_repository

let lift_string =
  (Lift.string empty_exception Public_data.empty_cursus_exception).Lift.safe

let mandatory_fields =
  [
    lift_pred (fun a -> a.code_cours) "The code gps of a course is missing";
  ]

let all_fields =
  let record_name = "a program exception" in
  [
    lift_string
      ~keyword:Public_data.Code_gps
      ~set_tmp:(fun state code_cours x ->
          state,
          let code_cours =
            match code_cours with
            | Some x when String.trim x = "" -> None
            | _ -> code_cours
          in
       {x with code_cours})
      ~get_tmp:(fun a -> a.code_cours)
      ~get:(fun a -> a.Public_data.codecours)
      ~set:(fun codecours a -> {a with Public_data.codecours})
      ~field_name:"GPS code"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.FirstName
      ~set_tmp:(fun state student_firstname x ->
        state,
        let student_firstname =
          match student_firstname with
          | Some x when String.trim x = "" -> None
          | _ -> student_firstname
        in
        {x with student_firstname})
      ~get_tmp:(fun a -> a.student_firstname)
      ~get:(fun a -> a.Public_data.student_firstname)
      ~set:(fun student_firstname a ->
         {a with Public_data.student_firstname})
      ~field_name:"the first name of the student"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.LastName
      ~set_tmp:(fun state student_lastname x ->
          state,
          let student_lastname =
            match student_lastname with
            | Some x when String.trim x = "" -> None
            | _ -> student_lastname
          in
          {x with student_lastname})
      ~get_tmp:(fun a -> a.student_lastname)
      ~get:(fun a -> a.Public_data.student_lastname)
      ~set:(fun student_lastname a ->
          {a with Public_data.student_lastname})
      ~field_name:"the last name of the student"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Annee_Academique
      ~set_tmp:(fun state annee_de_validation x ->
          state,
          let annee_de_validation =
            match annee_de_validation with
            | Some x when String.trim x = "" -> None
            | _ -> annee_de_validation
          in
          {x with annee_de_validation})
      ~get_tmp:(fun a -> a.annee_de_validation)
      ~get:(fun a -> a.Public_data.annee_de_validation)
      ~set:(fun annee_de_validation a ->
             {a with Public_data.annee_de_validation})
      ~field_name:"validation year"
      ~record_name
      ~pos:__POS__;
    lift_string
      ~keyword:Public_data.Departement
      ~set_tmp:(fun state dpt x ->
          state,
          let dpt =
            match dpt with
            | Some x when String.trim x = "" -> None
            | _ -> dpt
          in
          {x with dpt})
      ~get_tmp:(fun a -> a.dpt)
      ~get:(fun a -> a.Public_data.class_dpt)
      ~set:(fun class_dpt a-> {a with Public_data.class_dpt})
      ~field_name:"departement"
      ~record_name
      ~pos:__POS__ ;
    lift_string
      ~keyword:Public_data.Niveau
      ~set_tmp:(fun state course_level x ->
          state,
          let course_level =
            match course_level with
            | Some x when String.trim x = "" -> None
            | _ -> course_level
          in
          {x with course_level})
      ~get_tmp:(fun a -> a.course_level)
      ~get:(fun a -> a.Public_data.class_level)
      ~set:(fun class_level a -> {a with Public_data.class_level})
      ~field_name:"course level"
      ~record_name
      ~pos:__POS__ ;
  ]

let get_cursus_exceptions
    ?repository
    ?prefix
    ?file_name
    state
  =
  Scan_csv_files.collect_gen
    ~strict:true
    ?repository
    ?prefix
    ?file_name
    ~compute_repository
    ~fun_default:fun_ignore
    ~keywords_of_interest
    ~keywords_list
    ~init_state:empty_exception
    ~empty_elt:Public_data.empty_cursus_exception
    ~add_elt:Remanent_state.add_cursus_exception
    ~mandatory_fields
    ~all_fields
    ?event_opt
    state
