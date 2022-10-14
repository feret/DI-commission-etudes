module StringOptMap =
  Map_tools.MakeSimplified
    (
    struct
      module Ord   =
      struct
        type t = string option * string
        let compare1 a b =
          match a,b with
          | None, None -> 0
          | None, _ -> 1
          | _, None -> -1
          | Some "", Some "" -> 0
          | Some "", _ ->  1
          | _ , Some "" -> -1
          | Some "dens", Some "dens" -> 0
          | Some "dens", _ ->  1
          | _ , Some "dens" -> -1
          | Some a, Some b -> compare a b
        let compare a b =
          match compare1 (fst a) (fst b) with
          | 0 -> compare (snd b) (snd a)
          | x -> x
      end

      let simplify s =
        Special_char.lowercase
          (Special_char.correct_string_txt
             (String.trim s))
      let simplify (a,b) =
        Tools.map_opt simplify a,simplify b
    end)

let dpt_maths = "mathematiques"
let dpt_info = "informatique"
let dpt_phys = "physique"
let dpt_phyl = "phylosiphie"
let dpt_bio = "biologie"
let dpt_arts = "arts"
let dpt_lila = "litteratures et langage"
let dpt_ibens = dpt_bio

let dpt_dec = "etudes cognitives"
let dpt_eco = "economie"
let dpt_dri = "relations internationales"

let dpt_maths_en = "mathematics"
let dpt_info_en = "computer science"
let dpt_phys_en = "physics"
let dpt_phyl_en = "phylosophy"
let dpt_bio_en = "biology"
let dpt_arts_en = "arts"
let dpt_lila_en = "litteratures and language"
let dpt_ibens_en = dpt_bio_en

let dpt_dec_en = "cognitive sciences"
let dpt_eco_en = "economics"


let dpt_eco_gps_name = dpt_eco
let dpt_info_gps_name = dpt_info
let dpt_phys_gps_name = dpt_phys
let dpt_maths_gps_name = "mathematiques et applications"
let dpt_bio_gps_name = dpt_bio
let dpt_dri_gps_name = dpt_dri
let dpt_dec_gps_name = dpt_dec
let dpt_arts_gps_name  = dpt_arts
let dpt_lila_gps_name = dpt_lila

let acro_dpt_arts = "ARTS"
let acro_dpt_phys = "PHYS"
let acro_dpt_info = "DI"
let acro_dpt_maths = "DMA"
let acro_dpt_eco = "ECO"
let acro_dpt_bio = "BIO"
let acro_dpt_dri = "DRI"
let acro_dpt_lila = "LILA"

let dpt_arts_full = "Département d'Arts"
let dpt_info_full = "Département d'Informatique"
let dpt_maths_full = "Département de Mathématiques et Applications"
let dpt_phys_full = "Département de Physique"
let dpt_bio_full = "Institut de Biologie"
let dpt_dec_full = "Département d'Études Cognitives"
let dpt_eco_full = "Département d'Économie"
let dpt_dri_full = "Direction des Relations Internationales"
let dpt_lila_full = "Département de Litteratures et Langage"

let dpt_arts_full_en = "Arts Department"
let dpt_info_full_en = "Computer Science Department"
let dpt_maths_full_en = "Department of Mathematics and their Applications"
let dpt_phys_full_en = "Physics Department"
let dpt_bio_full_en = "Biology Institute"
let dpt_dec_full_en = "Cognitive Studies Department"
let dpt_eco_full_en = "Economy Department"
let dpt_dri_full_en = "International Relations Office"
let dpt_lila_full_en = "Litteratures and Language Department"

let simplify_string s =
  Special_char.lowercase
    (Special_char.correct_string_txt
       (Special_char.correct_string_utf8 (String.trim s)))

let acro_of_gps_name x =
  let x = simplify_string  x in
  if x = dpt_info_gps_name
  then acro_dpt_info
  else if x = dpt_maths_gps_name
  then acro_dpt_maths
  else if x = dpt_bio_gps_name
  then acro_dpt_bio
  else if x = dpt_eco_gps_name
  then acro_dpt_eco
  else if x = dpt_phys_gps_name
  then acro_dpt_phys
  else if x = dpt_dri_gps_name
  then acro_dpt_dri
  else if x = dpt_arts_gps_name
  then acro_dpt_arts
  else if x = dpt_lila_gps_name
  then acro_dpt_lila
  else acro_dpt_info

let addmap x data map =
  let old =
    match
      StringOptMap.find_opt x map
    with
    | Some x -> x
    | None -> []
  in
  StringOptMap.add x (data::old) map

let gen_year f pos msg state a_opt b_opt =
  match
    a_opt,b_opt
  with
  | None, _ -> state, b_opt
  | _, None -> state, a_opt
  | Some a, Some b ->
    try
      state,
      Some (string_of_int
              (f (int_of_string a) (int_of_string b)))
    with
    | _ ->
      Remanent_state.warn
        pos
        msg
        Exit
        state,
      a_opt

let min_year = gen_year min
let max_year = gen_year max

let addfirstlast state x dispense data map =
  let data = Some data in
  if dispense
  then
    match
      StringOptMap.find_opt x map
    with
    | None ->
      state, StringOptMap.add x (None,None) map
    | Some _ -> state, map
  else
    let state, answer =
      match
        StringOptMap.find_opt x map
      with
      | Some (a_opt,b_opt) ->
        let state, a_opt =
          min_year
            __POS__
            "internal error: both arguments of min_year should be convertible into integers"
            state a_opt data
        in
        let state, b_opt =
          max_year __POS__ "internal error: both arguments of max_year should be convertible into integers"
            state b_opt data
        in
        state, (a_opt,b_opt)
      | None  -> state,(data,data)
    in
    state, StringOptMap.add x answer map

let is_dens x =
  match x with
    Some "dens", _ -> true
  | _ -> false

let get_display_year pos msg state year x map =
  if is_dens x then state, year
  else
    begin
      match
        StringOptMap.find_opt
          x map
      with
      | None ->
        begin
          Remanent_state.warn
            pos
            msg
            Exit
            state, year
        end
      | Some (_,Some y)-> state, y
      | Some (_,None) ->
      begin
        Remanent_state.warn
          pos
          msg
          Exit
          state, year
      end
    end

let add_course pos msg state year  key courses map =
  match
    Public_data.YearMap.find_opt year map
  with
  | None ->
    begin
      Remanent_state.warn
        pos
        msg
        Exit
        state, map
    end
  | Some (a,b,d_map) ->
    let old =
      match
        StringOptMap.find_opt key d_map
      with
      | None -> []
      | Some old -> old
    in
    let d_map =
      StringOptMap.add key (List.concat [courses;old]) d_map
    in
    state, Public_data.YearMap.add year (a,b,d_map) map

type diplome =
  {
    grade: string option;
    niveau: string option;
    libelle: string option;
    etablissement: string option;
    discipline_SISE: string option;
    diplome_diplome: string option;
    obtenu_en: string option;
    directeur_sujet_de_recherche: string option;
  }

let log_string
    state (label,string_opt) =
  match string_opt with
  | None -> state
  | Some a ->
    let () =
      Remanent_state.log
        state "%s: %s; " label a
    in
    state

let log_note state  (label, note_opt) =
  let state, s_opt =
    match note_opt with
    | None -> state, None
    | Some a ->
      let state, s = Notes.to_string __POS__ state a in
      state, Some s
  in
  let state  =
    log_string
      state (label,s_opt)
  in
  state

let _log_int
    state (label,string_opt) =
  match string_opt with
  | None -> state
  | Some a ->
    let () =
      Remanent_state.log
        state "%s: %i" label a
    in
    state

let log_float
        state (label,string_opt) =
      match string_opt with
      | None -> state
      | Some a ->
        let () =
          Remanent_state.log
            state "%s: %f" label a
        in
        state

let log_bool
    state (label,string_opt) =
  match string_opt with
  | None -> state
  | Some a ->
    let () =
      Remanent_state.log
        state "%s: %s" label
        (if a
         then "Oui"
         else "Non")
    in
    state

let log_genre
    state (label, string_opt) =
  match string_opt with
  | None -> state
  | Some a ->
    let () =
      Remanent_state.log
        state "%s: %s" label
        (match a with
         | Public_data.Masculin -> "M"
         | Public_data.Feminin -> "F"
         | Public_data.Unknown -> "?")
    in state

let log_statut
        state (label, string_opt) =
      match string_opt with
      | None -> state
      | Some _ ->
        let () =
          Remanent_state.log
            state "%s: %s" label
            (Public_data.string_of_statut_opt string_opt)
        in state

let string_of_origin_short_opt a =
  match a with
  | None -> ""
  | Some
      ( Public_data.DensInfo
      | Public_data.DensDEC
      | Public_data.DensPhys
      | Public_data.DensMath) -> "universitaire"
  | Some Public_data.Nes -> "Normalien Étudiant Sciences"
  | Some Public_data.EchErasm -> "Erasmus"
  | Some Public_data.Info -> "Info"
  | Some Public_data.Mpi -> "MPI"
  | Some Public_data.Pc  -> "PC"
  | Some Public_data.ED386 -> "ED386"
  | Some Public_data.PensionnaireEtranger -> "Pensionnaire Étranger"
  | Some Public_data.Psi -> "PSI"
  | Some Public_data.Sis -> "SI"
  | Some Public_data.M_MPRI -> "MPRI"
  | Some Public_data.AL -> "Khâgne"
  | Some Public_data.BCPST -> "BCPST"

  let english_string_of_origin_short_opt a =
    match a with
    | None -> ""
    | Some
        ( Public_data.DensInfo
        | Public_data.DensDEC
        | Public_data.DensPhys
        | Public_data.DensMath) -> "university"
    | Some Public_data.Nes -> "Sciences Normalien Student"
    | Some Public_data.EchErasm -> "Erasmus"
    | Some Public_data.Info -> "CS"
    | Some Public_data.Mpi -> "Math-Phys-CS"
    | Some Public_data.Pc  -> "Phys-Chem"
    | Some Public_data.ED386 -> "ED386"
    | Some Public_data.PensionnaireEtranger -> "Int. Exchange"
    | Some Public_data.Psi -> "Phys-Eng"
    | Some Public_data.Sis -> "Int. Exchange"
    | Some Public_data.M_MPRI -> "MPRI"
    | Some Public_data.AL -> "Humanities"
    | Some Public_data.BCPST -> "Bio-Chem-Phys-Geo"

let log_origine
    state (label, string_opt) =
  match string_opt with
  | None -> state
  | Some _ ->
    let () =
      Remanent_state.log
        state "%s: %s" label
        (Public_data.string_of_origin_opt string_opt)
    in state

let log_diplome state diplome =
  let state =
    List.fold_left
      log_string
      state
      [
        "GRADE",diplome.grade;
        "NIVEAU",diplome.niveau;
        "LIBELLE",diplome.libelle;
        "ETABLISSEMENT",diplome.etablissement;
        "DISCIPLINE",diplome.discipline_SISE;
        "OBTENU EN",diplome.obtenu_en;
        "DIRECTEUR",diplome.directeur_sujet_de_recherche
      ]
  in
  let () =
    Remanent_state.log state "@."
  in
  let () =
    Remanent_state.flush state
  in
  state

let empty_diplome =
  {
    grade = None;
    niveau = None;
    libelle = None;
    diplome_diplome = None;
    etablissement = None;
    discipline_SISE = None;
    obtenu_en = None;
    directeur_sujet_de_recherche = None;

  }

type cours =
  {
    semestre: string option;
    code_cours: string option;
    responsable: string option;
    cours_libelle: string option;
    cours_etablissement: string option;
    duree: float option;
    ects: float option;
    diplome: string option;
    contrat: bool option;
    accord: bool option;
    note: Public_data.note option;
    lettre: string option;
    commentaire: string list
  }

let log_cours state cours =
  let state =
      List.fold_left
        log_string
        state
        [
          "SEMESTRE",cours.semestre;
          "CODE",cours.code_cours;
          "RESP",cours.responsable;
          "LIBELLE",cours.cours_libelle;
          "ETABLISSEMENT",cours.cours_etablissement
        ]
    in
    let state =
      List.fold_left
        log_float
        state
        [
          "DUREE",cours.duree;
          "ECTS",cours.ects
        ]
    in
    let state =
      log_string state
        ("DIPLOME",cours.diplome)
    in
    let state =
      List.fold_left
        log_bool
        state
        [
          "CONTRAT",cours.contrat;
          "ACCORD",cours.accord
        ]
    in
    let state =
      log_note state
        ("NOTE",cours.note)
    in
    let state =
      log_string
        state
        ("LETTRE",cours.lettre)
    in
    let () =
      List.iter
        (Remanent_state.log
           state "%s")
        cours.commentaire
    in
    let () =
      Remanent_state.log
        state "@."
    in
    let () =
      Remanent_state.flush state
    in
    state

let log_annee = log_string

let empty_cours =
  {
    semestre = None ;
    code_cours = None ;
    responsable = None ;
    cours_libelle = None ;
    cours_etablissement = None ;
    duree = None ;
    ects = None ;
    diplome = None ;
    contrat = None ;
    accord = None ;
    note = None ;
    lettre = None ;
    commentaire = [];
  }

type date =
  {
    jour:int;
    mois:int;
    an:int
  }
type stage =
  {
    id: string option;
    cvt: string option;
    periode: string option;
    date_debut: date option;
    date_fin: date option;
    sujet: string option;
    directeur_de_stage: string option;
    responsable_local: string option;
    service_labo_dpt: string option;
    etablissement_ou_entreprise: string option;
    stage_credits: float option;
    stage_valide: Public_data.valide option;
    stage_accord: bool option;
    stage_commentaire: string list;
    type_de_financement: string option;
    periode_de_financement: string option;
    organisme_de_financement: string option;
  }

let log_stage state stage =
  let state =
    List.fold_left
      log_string
      state
      [
        "periode", stage.periode;
        "sujet", stage.sujet;
        "directeur", stage.directeur_de_stage;
        "responsable local", stage.responsable_local;
        "service/labo/dpt", stage.service_labo_dpt;
        "etablissement ou entreprise", stage.etablissement_ou_entreprise]
  in
  let state =
    log_float state
      ("credits",stage.stage_credits)
  in
  let state, valide =
    match stage.stage_valide
    with
    | None -> state, None
    | Some v ->
      let a,s =
        Valide.to_string __POS__ state v
      in
      a, Some s
  in
  let state =
    log_string state
      ("validé", valide)
  in
  let state =
      log_bool
      state
      ("accord", stage.stage_accord)
  in
  let () =
    if stage.stage_commentaire = []
    then ()
    else
      let () = Remanent_state.log state "COMMENTAIRES :"
      in
      List.iter
        (Remanent_state.log state "%s")
        stage.stage_commentaire
  in
  List.fold_left
    log_string state
    [
      "type_de_financement",stage.type_de_financement;
      "periode_de_financement",stage.periode_de_financement;
      "organisme_de_financement", stage.organisme_de_financement
    ]

let empty_stage =
  {
    id = None;
    cvt = None ;
    date_debut = None ;
    date_fin = None ;
    periode = None;
    sujet = None;
    directeur_de_stage = None;
    responsable_local = None;
    service_labo_dpt = None;
    etablissement_ou_entreprise = None;
    stage_credits = None;
    stage_valide = None;
    stage_accord = None;
    stage_commentaire = [];
    type_de_financement = None;
    periode_de_financement = None;
    organisme_de_financement = None;
  }
type bilan_annuel =
       {
         annee: Public_data.annee option;
         situation_administrative: string option;
         programme_d_etudes: string option;
         derniere_annee: bool option;
         departement_principal: string option;
         departement_secondaire: string option;
         diplomes: diplome list;
         inscription_au_DENS: bool option;
         cours: cours list;
         code_option: string option;
         option: string option;
         nannee: int option;
         gpscodelist: string list ;
       }

let log_bilan_annuel state bilan =
  let state =
    log_annee
      state  ("annee",bilan.annee)
  in
  let state =
    List.fold_left
      log_string
      state
      [
        "situation administrative",
        bilan.situation_administrative;
        "programme d'études",bilan.programme_d_etudes;
        "département principal",
        bilan.departement_principal;
        "département secondaire",
        bilan.departement_secondaire;
        "code_option",
        bilan.code_option;
        "option",
        bilan.option]
  in
  let _ =
    log_bool state ("Inscription au DENS", bilan.inscription_au_DENS)
  in
  let state =
    List.fold_left
      (fun state diplome ->
         log_diplome state diplome)
      state
      bilan.diplomes
  in
  let state =
    List.fold_left
      (fun state cours ->
         log_cours state cours)
      state
      bilan.cours
  in
  state

let empty_bilan_annuel =
  {
    annee = None ;
    situation_administrative = None ;
    programme_d_etudes = None ;
    derniere_annee = None ;
    departement_principal = None ;
    departement_secondaire = None ;
    diplomes = [] ;
    inscription_au_DENS = None;
    cours = [];
    code_option = None;
    option=None;
    nannee=None;
    gpscodelist=[]
  }

type gps_file =
  {
    nom: string option;
    prenom: string option;
    nom_complet: string option;
    genre: Public_data.genre option ;
    date_de_naissance: string option;
    promotion: string option;
    origine: Public_data.origin option;
    statut: Public_data.statut option;
    annee_en_cours: Public_data.annee option;
    contact_ens: string option;
    tuteur: string option;
    situation: bilan_annuel Public_data.YearMap.t;
    stages: stage list;
  }


let need_a_mentor gps_file =
  match gps_file.statut with
    Some ( Public_data.Eleve_bis
         | Public_data.Eleve
         | Public_data.Etudiant
         | Public_data.Boursier_si) -> true
  | Some (
      Public_data.Ex_boursier_si
    | Public_data.Ex_eleve
    | Public_data.Ex_eleve_bis
    | Public_data.Ex_etudiant
    | Public_data.Ex_hors_GPS
    | Public_data.Hors_GPS) | None  -> false

let _log_gps_file state gps =
  let state =
    List.fold_left
      log_string
      state
      ["nom",gps.nom;
       "prenom",gps.prenom]
  in
  let state =
    log_genre
      state
      ("genre",gps.genre)
  in
  let state =
    List.fold_left
      log_string
      state
      [
        "nom complet",gps.nom_complet;
        "date de naissance",gps.date_de_naissance;
        "promotion",gps.promotion;
      ]
  in
  let state =
    log_origine
      state
      ("origine",gps.origine)
  in
  let state =
    log_statut
      state
      ("statut",gps.statut)
  in
  let state =
    log_annee
      state
      ("annee en cours",gps.annee_en_cours)
  in
  let state =
    List.fold_left
      log_string
      state
      [
        "contact_ens",gps.contact_ens;
        "tuteur",gps.tuteur
      ]
  in
  let state =
    Public_data.YearMap.fold
      (fun annee bilan state ->
         let state =
           log_string
             state
             ("annee",Some annee)
         in
         let state =
           log_bilan_annuel
             state
             bilan
         in
         state)
      gps.situation
      state
  in
  let state =
    List.fold_left
      log_stage
      state
      gps.stages
  in
  state

let empty_gps =
  {
    nom = None ;
    prenom = None ;
    nom_complet = None ;
    genre = None ;
    date_de_naissance = None ;
    promotion = None ;
    origine = None ;
    statut = None ;
    annee_en_cours = None ;
    contact_ens = None ;
    tuteur = None ;
    situation = Public_data.YearMap.empty ;
    stages = [];
  }

type remanent =
  {gps_file: gps_file;
   rem_cours: cours;
   rem_diplome: diplome;
   rem_commentaires: string list ;
   stage: stage;
   inscription_DENS: string option;
   sit_adm: string option;
   prg_et: string option;
   annee_de_travail : Public_data.annee option;
   last_year: bool option;
   dpt_principal: string option;
   dpt_secondaire: string option;
   opt: string option;
   prenote: string option;
   preaccord: bool option;
   prevalide: Public_data.valide option;
   precode: string option;
  }

let _get a b c =
  a.date_fin, b.jour, c.inscription_DENS

  let fetch_date pos state date =
    let list = String.split_on_char '/' date in
    match list with
      [d;m;y] ->
      begin
        try
          let d,m,y = int_of_string d,int_of_string m,int_of_string y in
          state,
          Some {jour=d;mois=m;an=y}
        with
          _ ->
          Remanent_state.warn_dft pos
            "Wrong string for a date"
            Exit None state
      end
    | _ -> Remanent_state.warn_dft pos
             "Wrong string for a date"
             Exit None state

  let fetch_deb_fin pos state periode =
    let list = String.split_on_char ' ' periode in
    match list with
      ["du";date_deb;"au";date_fin]
      ->

      let state, deb = fetch_date pos state date_deb in
      let state, fin = fetch_date pos state date_fin in
      state, Some (deb,fin)
    | _ ->
      Remanent_state.warn_dft
        pos
        "wrong string for the period of an internship"
        Exit
        None
        state

let filter_stage_year year state stages =
  try
    let year = int_of_string year in
    state, List.filter
      (fun stage ->
         match stage.date_debut with
         | None -> true
         | Some date ->
           (date.mois>8 && date.an=year) || (date.mois<=8 && date.an=year+1)
      )
      stages
  with
  | _ ->
    let msg =
      Format.sprintf
        "Wrong string for an academic year (%s)"
        year
    in
    Remanent_state.warn_dft
      __POS__
      msg
      Exit
      stages
      state

  let fetch_string string comment =
    let list =
      List.rev_map
        (String.split_on_char ' ')
        (List.rev comment)
    in
    let list = List.flatten list in
    let list =
      List.fold_left
        (fun residue elt ->
           let list =
             String.split_on_char '/' elt
           in
           list::residue
        )
        [] (List.rev list)
    in
    let list = List.flatten list in
    let rec aux string list =
      match list with
      | [] -> None
      | h::d::_ when h=string -> Some d
      | _::t -> aux string t
    in
    aux string list

let fetch_id = fetch_string "id"
let fetch_cvt = fetch_string "cvt"

let get_bilan_annuel state remanent year =
  state,
  match
    Public_data.YearMap.find_opt
      year
      remanent.gps_file.situation
  with
  | Some a -> a
  | None ->
    empty_bilan_annuel

let set_bilan_annuel state remanent year bilan =
  let situation =
    Public_data.YearMap.add
      year
      bilan
      remanent.gps_file.situation
  in
  let gps_file =
    {remanent.gps_file with situation}
  in
  state, {remanent with gps_file}

let store_gen
    get get_current set
    pos ~who state current_file current_file' output  =
  let state, current_file =
    match
      current_file'.annee_de_travail
    with
    | None ->
      Remanent_state.warn_dft
                 pos
                 "Current year is not documented"
                 Exit
                 current_file'
                 state
    | Some year ->
      let state, bilan =
        get_bilan_annuel
          state
          current_file
          year
      in
      let state, current_file'' =
        get_current state pos ~who current_file'
      in
      let state, bilan' = get state bilan in
      let updated =
        (current_file'')::bilan'
      in
      let state, bilan =
        set state bilan updated
      in
      set_bilan_annuel
        state
        current_file
        year
        bilan
  in
  state,
  current_file,
  output

let store_diplome =
  store_gen
    (fun state bilan -> state, bilan.diplomes)
    (fun state pos ~who remanent ->
       let _ = who,pos  in
       state, remanent.rem_diplome)
    (fun state bilan diplomes -> state, {bilan with diplomes})

let get_compensation
    ~firstname ~lastname ~year ~codecours note
    state
  =
  match
    Remanent_state.get_compensation
      ~firstname ~lastname ~year ~codecours
      state
  with
  | state, None -> state, false
  | state, _
    ->
    begin
      match note with
      | Some note ->
        if Notes.compensable note
        then
          state, true
        else
          let state, note_string = Notes.to_string __POS__ state note in
          let msg =
            Printf.sprintf
              "Note %s (%s %s ANNEE:%s CODE_GPS:%s) needs no compensation"
              note_string
              firstname
              lastname
              year
              codecours
          in
          Remanent_state.warn_dft
            __POS__
            msg
            Exit
            false
            state
      | None ->
        let msg =
          Printf.sprintf
            "Missing notes cannot be compensated (%s %s ANNEE:%s CODE_GPS:%s)"
            firstname
            lastname
            year
            codecours
        in
        Remanent_state.warn_dft
          __POS__
          msg
          Exit
          false
          state
    end


let store_cours  =
  store_gen
    (fun state bilan -> state, bilan.cours)
    (fun state pos ~who remanent ->
       let who =
         match remanent.rem_cours.cours_libelle
         with
         | None -> who
         | Some c ->
           if String.trim c = ""
           then
             who
           else
         Printf.sprintf
           "%s in %s" who c
       in
       let state, note  =
         match remanent.prevalide, remanent.prenote with
         | Some (Public_data.Bool false | Public_data.Abs), None
           ->
           state, Some Public_data.Absent
         | Some Public_data.Abs, Some i ->
           let msg =
             Format.sprintf
               "Note and validity status are incompatible (validation:abs, note:%s) for %s" i who
           in
           Remanent_state.warn_dft
             pos
             msg
             Exit
             (Some Public_data.Absent)
             state
         | Some Public_data.Bool true, None ->
           state, (Some Public_data.Valide_sans_note)
         | v , Some n ->
           begin
             let state =
               match v with
               | Some _ -> state
               | None ->
                 let msg =
                   Format.sprintf
                     "Undefined validity status for note %s for %s"
                     n who
                 in
                 Remanent_state.warn
                   pos
                   msg
                   Exit
                   state
                   in
             let state, note_opt = Notes.of_string __POS__ state n v in
             match note_opt with
             | None ->
               let msg =
                 Format.sprintf
                   "Invalid content for the field note for %s" who
               in
               Remanent_state.warn_dft
                 pos
                 msg
                 Exit
                 note_opt
                 state
             | Some note ->
               if
                 (match v with
                 | Some v -> Notes.valide note = Valide.valide v
                 | None -> true)
               then
                 state, note_opt
               else
                 let state, compensation =
                     match
                       remanent.gps_file.prenom,
                       remanent.gps_file.nom,
                       remanent.annee_de_travail,
                       remanent.precode
                     with
                       Some firstname, Some lastname,
                       Some year, Some codecours ->
                         get_compensation
                           ~firstname ~lastname ~year  ~codecours
                           note_opt state
                     | _ ->
                     let state =
                       Remanent_state.warn
                         __POS__
                         "Some information are missing to check compensation"
                         Exit
                         state
                     in state, false
                   in
                   if compensation then state, note_opt
                   else
                     begin
                       let state, note_string =
                         Notes.to_string __POS__ state note
                       in
                       let state, v_string =
                         match v with None -> state, "undefined"
                                    | Some v ->
                                      Valide.to_string
                                        __POS__
                                        state
                                        v
                       in
                       let msg =
                         Format.sprintf
                           "Note %s and validity status %s are  incompatible for %s"
                           note_string v_string who
                       in
                       Remanent_state.warn_dft
                         __POS__
                         msg
                         Exit
                         note_opt
                         state

           end
           end
         | None, None ->
           state, Some Public_data.En_cours
       in
       let code_cours = remanent.precode in
       let accord = remanent.preaccord in
       let commentaire = remanent.rem_commentaires in
       state, {remanent.rem_cours with note ; code_cours ; accord ; commentaire})
    (fun state bilan cours -> state, {bilan with cours})

let add_extra_course state cours_a_ajouter gps_file =
  let situation = gps_file.situation in
  let bilan =
    match
      Public_data.YearMap.find_opt
        cours_a_ajouter.Public_data.coursaj_annee
        situation
    with
    | None -> empty_bilan_annuel
    | Some b -> b
  in
  let elt =
    {
      semestre = None ;
      code_cours =
        begin
          match cours_a_ajouter.Public_data.coursaj_code with
          | None -> Some " "
          | a -> a
        end;
      responsable = None ;
      cours_libelle = Some (String.trim (cours_a_ajouter.Public_data.coursaj_libelle));
      cours_etablissement = None ;
      duree = None ;
      ects = Some cours_a_ajouter.Public_data.coursaj_ects;
      diplome = Some cours_a_ajouter.Public_data.coursaj_level ;
      contrat = None ;
      accord = Some true ;
      note =
        (match cours_a_ajouter.Public_data.coursaj_note
        with
        | Some f ->
          Some (Public_data.Float f)
        | None ->
          Some (Public_data.Valide_sans_note)) ;
        lettre = None;
      commentaire = [];
    }
  in
  let bilan = {bilan with cours = elt::bilan.cours} in
  state,
  {gps_file with
   situation =
     Public_data.YearMap.add
       cours_a_ajouter.Public_data.coursaj_annee
       bilan gps_file.situation
  }

let store_stage pos ~who state current_file current_file' output=
  let _ = pos, who in
  let stage = current_file'.stage in
  let state, date_opt =
    match stage.periode with
    | None -> state, None
    | Some periode -> fetch_deb_fin pos state periode
  in
  let date_debut, date_fin =
    match
      date_opt
    with
    | None -> None, None
    | Some (a,b) -> a,b
  in
  let id = fetch_id current_file'.rem_commentaires in
  let cvt = fetch_cvt current_file'.rem_commentaires in
  let stage = {stage with stage_valide = current_file'.prevalide ;
                          stage_accord = current_file'.preaccord ;
                          id ; cvt ; date_debut ; date_fin ;
                          stage_commentaire = current_file'.rem_commentaires}
  in
  let stages = current_file.gps_file.stages in
  let stages = stage::stages in
  let gps_file = {current_file.gps_file with stages} in
  state, {current_file with gps_file}, output

let store_gen_fields
    list_string list_bool pos state current_file current_file' output
  =
  let state, current_file =
    match
      current_file'.annee_de_travail
    with
    | None ->
      Remanent_state.warn_dft
        pos
        "Current year is not documented"
        Exit
        current_file'
        state
    | Some year ->
      let state, bilan =
        get_bilan_annuel
          state
          current_file
          year
      in
      let updated =
        List.fold_left
          (fun bilan (get,set) ->
             set (get current_file') bilan)
          bilan
          list_string
      in
      let updated =
        List.fold_left
          (fun bilan (get,set) ->
             set (get current_file') bilan)
          updated
          list_bool
      in
      set_bilan_annuel
        state
        current_file
        year
        updated
  in
  state,
  current_file,
  output

let set_dens
    state annee remanent
  =
  let state, remanent =
    match
      annee
    with
    | None -> state, remanent
    | Some year ->
      let state, bilan =
        get_bilan_annuel
          state
          remanent
          year
      in
      let updated =
        {bilan with inscription_au_DENS = Some true}
      in
      set_bilan_annuel
        state
        remanent
        year
        updated
  in
  state,
  remanent


let store_situation_adm
  =
  store_gen_fields
    [
      (fun x -> x.sit_adm),
      (fun situation_administrative x ->
         {x with situation_administrative});
      (fun x -> x.prg_et),
      (fun programme_d_etudes x -> {x with programme_d_etudes})]
    [
      (fun x -> x.last_year),
      (fun derniere_annee x -> {x with derniere_annee})]


let store_dpts
  =
  store_gen_fields
    [
      (fun x -> x.dpt_principal),
      (fun departement_principal x -> {x with departement_principal});
      (fun x -> x.dpt_secondaire),
      (fun departement_secondaire x -> {x with departement_secondaire});
    ]
    []

let store_option
  =
  store_gen_fields
    [
      (fun x -> x.precode),
      (fun code_option x -> {x with code_option});
      (fun x -> x.opt),
      (fun option x -> {x with option});
    ]
    []


let empty_remanent =
  {
    gps_file = empty_gps ;
    rem_cours = empty_cours ;
    rem_commentaires = [] ;
    stage = empty_stage ;
    rem_diplome = empty_diplome ;
    inscription_DENS = None ;
    sit_adm = None ;
    prg_et = None ;
    last_year = None ;
    opt = None ;
    dpt_principal = None ;
    dpt_secondaire = None ;
    annee_de_travail = None ;
    prenote = None ;
    prevalide = None ;
    precode = None ;
    preaccord = None ;
  }


let fun_default =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.LastName;
    Public_data.FirstName;
    Public_data.Genre;
    Public_data.FullName;
    Public_data.Date_de_Naissance;
    Public_data.Promo;
    Public_data.Origine;
    Public_data.Statut;
    Public_data.Annee_en_Cours;
    Public_data.Contact_ENS;
    Public_data.Tuteur;
    Public_data.Situation;
    Public_data.Annee_Academique;
    Public_data.Programme_d_etude;
    Public_data.Derniere_Annee;
    Public_data.Departements;
    Public_data.Departement_principal;
    Public_data.Departement_secondaire;
    Public_data.Options;
    Public_data.Code;
    Public_data.Option;
    Public_data.Diplomes;
    Public_data.Profil;
    Public_data.Grade;
    Public_data.Niveau;
    Public_data.Diplome;
    Public_data.Libelle;
    Public_data.Etablissement;
    Public_data.Discipline_SISE;
    Public_data.Obtenu_en;
    Public_data.Directeur_Sujet;
    Public_data.Enseignements;
    Public_data.Inscrit_au_DENS_en;
    Public_data.Semestre;
    Public_data.Responsable;
    Public_data.Duree;
    Public_data.ECTS;
    Public_data.Pour_Diplome;
    Public_data.Contrat;
    Public_data.Accord;
    Public_data.Valide;
    Public_data.Note;
    Public_data.Lettre;
    Public_data.Commentaire;
    Public_data.Stages_et_Sejours_a_l_Etranger;
    Public_data.Periode;
    Public_data.Sujet_du_Stage_Type_du_Sejour;
    Public_data.Directeur_de_Stage;
    Public_data.Responsable_local;
    Public_data.Service_Labo_Dpt;
    Public_data.Etablissement_ou_Entreprise;
    Public_data.Credits;
    Public_data.Valide;
    Public_data.Accord;
    Public_data.Type_de_Financement;
    Public_data.Periode_de_Financement;
    Public_data.Organisme_de_Financement;
  ]

let keywords_of_interest =
  [
  Public_data.Situation;
  Public_data.Programme_d_etude;
  Public_data.Derniere_Annee;
  Public_data.Departement_principal;
  Public_data.Code;
  Public_data.Option;
  Public_data.Grade;
  Public_data.Niveau;
  Public_data.Diplome;
  Public_data.Libelle;
  Public_data.Etablissement;
  Public_data.Discipline_SISE;
  Public_data.Obtenu_en;
  Public_data.Directeur_Sujet;
  Public_data.Inscrit_au_DENS_en;
  Public_data.Semestre;
  Public_data.Responsable;
  Public_data.Duree;
  Public_data.ECTS;
  Public_data.Pour_Diplome;
  Public_data.Contrat;
  Public_data.Accord;
  Public_data.Valide;
  Public_data.Note;
  Public_data.Lettre;
  Public_data.Periode;
  Public_data.Sujet_du_Stage_Type_du_Sejour;
  Public_data.Directeur_de_Stage;
  Public_data.Responsable_local;
  Public_data.Service_Labo_Dpt;
  Public_data.Etablissement_ou_Entreprise;
  Public_data.Credits;
  Public_data.Valide;
  Public_data.Accord;
  Public_data.Type_de_Financement;
  Public_data.Periode_de_Financement;
  Public_data.Organisme_de_Financement;
]

let bool_opt_of_string_opt pos state s_opt =
match s_opt with
| None -> state, None
| Some s ->
  if String.trim s = "" || s="?"
  then state, None
  else
    let s = Special_char.lowercase s in
    if
      List.mem
      s ["o";"oui";"y";"yes";"ok"]
    then state, Some true
    else if
      List.mem
        s
        ["n";"non";"no";"abs";"absent"]
    then state, Some false
    else
      let msg =
        Format.sprintf
          "Ill-formed Boolean (%s)"
          s
      in
      Remanent_state.warn_dft
        pos
        msg
        Exit
        None
        state

let genre_opt_of_string_opt pos state s_opt =
  match s_opt with
  | None -> state, None
  | Some s' ->
    let s = String.trim s' in
    if s = ""
    then
      state, None
    else
      let s = Special_char.lowercase s in
      if
        List.mem
          s ["m";"mr";"monsieur";"m.";"mr."]
      then
          state, Some Public_data.Masculin
      else if
        List.mem
          s
          ["mlle.";"mme.";"mlle";"mme";"madame";"mademoiselle"]
      then
        state, Some Public_data.Feminin
      else
        let msg =
          Format.sprintf
            "Ill-formed Gender (%s)"
            s
        in
        Remanent_state.warn_dft
          pos
          msg
          Exit
          None
          state

let statuts =
  [
    Public_data.Eleve,["n";"eleve";"normalien"];
    Public_data.Etudiant,["e";"etudiant"];
    Public_data.Eleve_bis,["eleve bis"];
    Public_data.Ex_eleve,["ex - eleve"];
    Public_data.Ex_eleve,["ex - etudiant"];
    Public_data.Ex_eleve_bis,["ex - eleve bis"];
    Public_data.Ex_boursier_si,["ex - boursier si"];
    Public_data.Boursier_si,["boursier si"];
    Public_data.Hors_GPS,["hors gps"];
    Public_data.Ex_hors_GPS,["ex - hors gps"]
  ]

let gen_fetch_opt_of_string_opt ?who ?gps_file list err pos state s_opt =
  match s_opt with
  | None -> state, None
  | Some s ->
    if String.trim s = ""
    then state, None
    else
      let s = simplify_string s in
      let rec aux state l =
        match l with
        | [] ->
          let who =
            match who with
            | Some who -> who
            | None ->
              begin
                match gps_file with
                | None -> ""
                | Some gps ->
                    Format.sprintf
                      "%s %s %s"
                      (Tools.unsome_string gps.nom)
                      (Tools.unsome_string gps.prenom)
                      (Tools.unsome_string gps.promotion)
              end
          in
          let who =
            let who = String.trim who in
            if who = "" then ""
            else Format.sprintf " for %s" who
          in
          let msg =
            Format.sprintf
              "Ill-formed %s (%s%s)"
              err s
              who
          in
          Remanent_state.warn_dft
            pos
            msg
            Exit
            None
            state
        | (statut,keywords)::t ->
          begin
            if List.mem s keywords
            then state, Some statut
            else aux state t
          end
      in
      aux state list

let statut_opt_of_string_opt ?who ?gps_file  =
  gen_fetch_opt_of_string_opt ?who ?gps_file statuts "status"

let origines =
  [
    Public_data.AL,["a/l"];
    Public_data.DensInfo,["dens-info"];
    Public_data.DensMath,["dens-dma"];
    Public_data.DensPhys,["dens-phys"];
    Public_data.Nes,["nes"];
    Public_data.DensDEC,["dens-dec"];
    Public_data.EchErasm,["e-echerasm"];
    Public_data.Info,["info"];
    Public_data.Mpi,["mpi"];
    Public_data.BCPST,["bcpst"];
    Public_data.PensionnaireEtranger,["e-pe"];
    Public_data.Pc,["pc"];
    Public_data.Psi,["psi"];
    Public_data.Sis,["sis"];
    Public_data.M_MPRI,["m-mpri"];
    Public_data.ED386,["ed-386"]
  ]

let concours =
  [
    Public_data.DensInfo,[];
    Public_data.EchErasm,[];
    Public_data.Info,["c-info"];
    Public_data.Mpi,["c-mpi"];
    Public_data.BCPST,["c-bcpst"];
    Public_data.PensionnaireEtranger,[];
    Public_data.Pc,[];
    Public_data.Psi,[];
    Public_data.Sis,["SI-S"];
    Public_data.M_MPRI,[]
  ]

let origin_opt_of_string_opt ?who ?gps_file =
  gen_fetch_opt_of_string_opt ?who ?gps_file origines "origin"

let origin_opt_of_concours ?who =
  gen_fetch_opt_of_string_opt ?who concours "concours"

let float_opt_of_string_opt _pos state s_opt =
  let state, float_opt_opt =
    Tools.map_opt_state
          (Notes.float_of_string __POS__)
          state s_opt
  in
  let float_opt =
    match float_opt_opt with
    | None -> None
    | Some a -> a
  in
  state, float_opt

let lift_gen get set  f state data remanent =
  let obj = f data (get remanent) in
  state, set obj remanent

let lift_gen_state  get set  f state data remanent =
    let state, obj = f state data (get remanent) in
    state, set obj remanent

let lift_gps =
  lift_gen
    (fun x -> x.gps_file)
    (fun gps_file remanent ->
       {remanent with gps_file})

let lift_gps_state =
  lift_gen_state
    (fun x -> x.gps_file)
    (fun gps_file remanent ->
       {remanent with gps_file})

let lift_cours_state =
  lift_gen_state
    (fun x -> x.rem_cours)
    (fun rem_cours remanent ->
       {remanent with rem_cours})

let lift_cours =
  lift_gen
    (fun x -> x.rem_cours)
    (fun rem_cours remanent ->
       {remanent with rem_cours})

let lift_stage_state =
  lift_gen_state
    (fun x -> x.stage)
    (fun stage remanent ->
       {remanent with stage})

let lift_stage =
  lift_gen
    (fun x -> x.stage)
    (fun stage remanent ->
       {remanent with stage})

let lift_diplome =
  lift_gen
    (fun x -> x.rem_diplome)
    (fun rem_diplome remanent ->
       {remanent with rem_diplome})

let asso_list =
    [
      Public_data.LastName,
      lift_gps
        (fun nom gps_file ->
           {gps_file with nom});
      Public_data.FirstName,
      lift_gps
        (fun prenom gps_file ->
           {gps_file with prenom});
      Public_data.Genre,
      lift_gps_state
        (fun state genre gps_file ->
           let state, genre =
             genre_opt_of_string_opt __POS__ state genre
           in
           state,{gps_file with genre});
      Public_data.FullName,
           lift_gps
             (fun nom_complet gps_file ->
                {gps_file with nom_complet});
      Public_data.Date_de_Naissance,
      lift_gps
        (fun date_de_naissance gps_file
          -> {gps_file with date_de_naissance});
      Public_data.Promo,
      lift_gps
        (fun promotion gps_file ->
           {gps_file with promotion});
      Public_data.Origine,
      lift_gps_state
        (fun state origine gps_file ->
          let state, origine =
            origin_opt_of_string_opt __POS__ ~gps_file state origine
          in
          state,{gps_file with origine});
      Public_data.Statut,
      lift_gps_state
        (fun state genre gps_file ->
           let state, statut =
             statut_opt_of_string_opt __POS__ ~gps_file state genre
           in
           state,{gps_file with statut});
      Public_data.Annee_en_Cours,
      lift_gps
        (fun annee_en_cours gps_file ->
           {gps_file with annee_en_cours});
      Public_data.Contact_ENS,
      lift_gps
        (fun contact_ens gps_file ->
           {gps_file with contact_ens});
      Public_data.Tuteur,
      lift_gps
        (fun tuteur gps_file ->
           {gps_file with tuteur});
      Public_data.Situation,
      (fun state sit_adm remanent ->
         state,
          {remanent with sit_adm});
      Public_data.Annee_Academique,
      (fun state annee_de_travail remanent ->
         state,
         {remanent with annee_de_travail});
      Public_data.Programme_d_etude,(fun state prg_et remanent ->
          state,
          {remanent with prg_et});
      Public_data.Derniere_Annee,
      (fun state last_year remanent ->
          let state, last_year =
            bool_opt_of_string_opt __POS__ state last_year
          in
          state, {remanent with last_year});
      Public_data.Departements,fun_default;
      Public_data.Departement_principal,
      (fun state dpt_principal remanent ->
         state,
         {remanent with dpt_principal});
      Public_data.Departement_secondaire,
      (fun state dpt_secondaire remanent ->
         state,
         {remanent with dpt_secondaire});
      Public_data.Options,
      fun_default;
      Public_data.Code,
        (fun state precode remanent ->
           state, {remanent with precode});
      Public_data.Option,
      (fun state opt remanent ->
         state, {remanent with opt});
      Public_data.Diplomes,fun_default;
      Public_data.Grade,
      lift_diplome
        (fun grade diplome ->
           {diplome with grade});
      Public_data.Diplome,
           lift_diplome
             (fun diplome_diplome diplome ->
                {diplome with diplome_diplome});
      Public_data.Niveau,
      lift_diplome
        (fun niveau diplome ->
           {diplome with niveau});
      Public_data.Libelle,
      (fun state l remanent ->
        let state, remanent =
          (lift_diplome
             (fun libelle diplome ->
                {diplome with libelle}))
             state l remanent
        in
        (lift_cours
           (fun cours_libelle cours ->
              let cours_libelle =
                match cours_libelle with
                | None -> None
                | Some a -> Some (String.trim a)
              in
            {cours with cours_libelle}))
          state l remanent)
      ;
      Public_data.Etablissement,
      lift_diplome
        (fun etablissement diplome ->
           {diplome with etablissement});
      Public_data.Discipline_SISE,
      lift_diplome
        (fun discipline_SISE diplome ->
           {diplome with discipline_SISE});
      Public_data.Obtenu_en,
      lift_diplome
        (fun obtenu_en diplome ->
           {diplome with obtenu_en});
      Public_data.Directeur_Sujet,
      lift_diplome
        (fun directeur_sujet_de_recherche diplome ->
           {diplome with directeur_sujet_de_recherche});
      Public_data.Enseignements,fun_default;
      Public_data.Inscrit_au_DENS_en,
      set_dens;
      Public_data.Semestre,
      lift_cours
        (fun semestre cours ->
           {cours with semestre});
      Public_data.Code,
      lift_cours
        (fun code_cours cours ->
           {cours with code_cours});
      Public_data.Responsable,
      lift_cours
        (fun responsable cours ->
           {cours with responsable});
      Public_data.Duree,
      lift_cours_state
        (fun state d cours ->
           let state, duree =
             float_opt_of_string_opt __POS__ state d
           in
           state, {cours with duree});
      Public_data.ECTS,
      lift_cours_state
        (fun state e cours ->
          let state, ects =
            float_opt_of_string_opt __POS__ state e
          in
          state, {cours with ects});
      Public_data.Pour_Diplome,
      lift_cours
        (fun diplome cours ->
           {cours with diplome});
      Public_data.Contrat,
      lift_cours_state
        (fun state data cours ->
           let state, contrat =
           bool_opt_of_string_opt __POS__ state data
           in
           state, {cours with contrat});
      Public_data.Accord,
        (fun state data remanent ->
          let state, preaccord =
            bool_opt_of_string_opt __POS__ state data
          in
            state, {remanent with preaccord } )
      ;
      Public_data.Valide,
        (fun state data remanent ->
           match data with
           | None -> state, remanent
           | Some data ->
             let context =
               Some (Format.sprintf "%s %s (%s)"
                       (Tools.unsome_string remanent.gps_file.nom)
                       (Tools.unsome_string remanent.gps_file.prenom)
                       (Tools.unsome_string remanent.gps_file.promotion))
             in
             let state, prevalide =
               Valide.of_string __POS__ ?context state data
             in
             state, {remanent with prevalide});
      Public_data.Note,
        (fun state data remanent ->
           let state, prenote =
              match data with
                | None -> state, remanent.prenote
                | Some s when String.trim s = "" -> state, remanent.prenote
                | Some s -> state, Some s
           in
           state, {remanent with prenote});
      Public_data.Lettre,
      lift_cours
        (fun lettre cours -> {cours with lettre});
      Public_data.Commentaire,
      (fun state data remanent ->
         match data with
         | None -> state, remanent
         | Some com ->
           if String.trim com = ""
           then state, remanent
           else
             let rem_commentaires = com::remanent.rem_commentaires in
             state, {remanent with rem_commentaires});
      Public_data.Stages_et_Sejours_a_l_Etranger,fun_default;
      Public_data.Periode,
      lift_stage
        (fun periode stage -> {stage with periode});
      Public_data.Sujet_du_Stage_Type_du_Sejour,
      lift_stage
        (fun sujet stage -> {stage with sujet});
      Public_data.Directeur_de_Stage,
      lift_stage
        (fun directeur_de_stage stage ->
           {stage with directeur_de_stage});
      Public_data.Responsable_local,
      lift_stage
        (fun responsable_local stage ->
           {stage with responsable_local});
      Public_data.Service_Labo_Dpt,
      lift_stage
        (fun service_labo_dpt stage ->
           {stage with service_labo_dpt});
      Public_data.Etablissement_ou_Entreprise,
      lift_stage
        (fun etablissement_ou_entreprise stage ->
         {stage with etablissement_ou_entreprise});
      Public_data.Credits,
      lift_stage_state
        (fun state c stage ->
          let state, stage_credits =
            float_opt_of_string_opt __POS__ state c
          in
          state, {stage with stage_credits});
      Public_data.Type_de_Financement,
      lift_stage
        (fun type_de_financement stage ->
         {stage with type_de_financement});
      Public_data.Periode_de_Financement,
      lift_stage
        (fun periode_de_financement stage ->
           {stage with periode_de_financement});
      Public_data.Organisme_de_Financement,
      lift_stage
        (fun organisme_de_financement stage ->
         {stage with organisme_de_financement})
    ]

let asso_list =
  List.rev_map
    (fun (a,b) ->
       {Keywords_handler.key=a;
       Keywords_handler.store=b})
    (List.rev asso_list)
let get_gps_file
  ~input
  state
  =
  let (rep,file)=input in
  let file_name =
    if rep = ""
    then
      file
    else
      Printf.sprintf "%s/%s" rep file
  in
  let who = file_name in
  let event_opt =
    Some
      (Profiling.Export_transcript
         (Some file_name))
  in
  let state =
      Remanent_state.open_event_opt
        event_opt
        state
  in
  let at_end_of_array_line
      header state current_file current_file' output =
    if
      List.mem
        (Some Public_data.Periode)
        header
    then
      store_stage
        __POS__ ~who
        state
        current_file
        current_file'
        output
    else if
      List.mem
        (Some Public_data.Note)
        header
    then
        store_cours
          __POS__ ~who
          state
          current_file
          current_file'
          output
    else if
        List.mem
        (Some Public_data.Situation)
        header
    then
      store_situation_adm
        __POS__
        state
        current_file
        current_file'
        output
    else if
      List.mem
        (Some Public_data.Departement_principal)
         header
    then
      store_dpts
        __POS__
        state
        current_file
        current_file'
        output
    else if
      List.mem
        (Some Public_data.Option) header
      || List.mem
        (Some Public_data.Code)
        header
    then
      store_option
        __POS__
        state
        current_file
        current_file'
        output
    else if
      List.mem
        (Some Public_data.Diplome) header
    then
      if
        match
          current_file'.rem_diplome.grade
        with
        | None -> false
        | Some x -> simplify_string x = "doctorat"
      then
        state, current_file, output
      else
        store_diplome
        __POS__ ~who
        state
        current_file
        current_file'
        output
    else
      Remanent_state.warn
        __POS__
        "Unrecognised array header"
        Exit state,
      current_file, output
  in
  let at_end_of_array
      _header state current_file output =
    state, current_file, output
  in
  let at_end_of_file state current_file output =
    state, current_file::output
  in
  let flush state _current_file output =
    state, output
in
let state, automaton  =
  Keywords_handler.make
    state
    {
      Keywords_handler.keywords = keywords_list ;
      Keywords_handler.all_fields = asso_list ;
      Keywords_handler.default = fun_default ;
      Keywords_handler.of_interest = keywords_of_interest;
      Keywords_handler.shared_functions =
        {
          Keywords_handler.do_at_end_of_array = at_end_of_array;
          Keywords_handler.do_at_end_of_file = at_end_of_file;
          Keywords_handler.do_at_end_of_array_line = at_end_of_array_line;
          Keywords_handler.flush = flush ;
        }
    }
in
let state, list =
  Scan_csv_files.get_list_from_a_file
    automaton
    empty_remanent
    state
    input
    []
in
let state, output =
  match
    list
  with
  | [a] ->
    state, Some a.gps_file
  | _ ->
    let msg =
      Printf.sprintf
        "ill-formed output (%i elements instead of 1)"
        (List.length list)
    in
    Remanent_state.warn_dft
      __POS__
      msg
      Exit
      None
      state
in
let state =
    Remanent_state.close_event_opt
      event_opt
      state
in
state, output

let filter_stage_string string get comment state stage =
  let id = fetch_string string comment in
  match id with
  | None -> state, stage
  | Some data ->
  state, List.filter
    (fun stage ->
       match get stage with
       | None -> true
       | Some a -> a=data)
    stage

let filter_stage_id = filter_stage_string "id" (fun stage -> stage.id)
let filter_stage_cvt = filter_stage_string "cvt" (fun stage -> stage.cvt)

let fetch_stage
    ~internship state commentaires stages =
  let year = internship.Public_data.missing_internship_year in
  let rec aux state fun_list stages =
    match fun_list with
    | [] ->
      let state =
        Remanent_state.add_ambiguous_internship_description
          state
          internship
      in
      state, (Some (List.hd stages))
    | filter::tail ->
      begin
        match filter state stages with
        | state, [] ->
          let state =
              Remanent_state.add_missing_internship_description
                state internship
          in
          state, None
        | state, [a] -> state, Some a
        | state, list -> aux state tail list
      end
  in
  aux
    state
    [filter_stage_year year ;filter_stage_cvt commentaires ;filter_stage_id commentaires]
    stages

let lgen _grade gps dpt acro d =
    List.exists
      (fun diplome ->
        List.exists
          (fun gps -> diplome.diplome_diplome=Some gps)
          gps)
      d.diplomes
    ||
    begin
      d.nannee = Some 1
      &&
      dpt <> ""
      &&
      (match d.departement_principal,d.departement_secondaire with
       | None, None -> false
       | Some x, None | None, Some x ->
         simplify_string x = dpt
       | Some x, Some y ->
         simplify_string x = dpt ||
            begin
simplify_string y = dpt
            && match acro with None -> true
| Some x ->
  List.exists (fun cours ->
Tools.substring x (match cours.code_cours with None -> "" | Some x -> x)&& cours.diplome = Some "L") d.cours
            end )
    end

let code_mandatory_course_DI_maths year code_cours =
  let i = int_of_string year in
  if i <= 2015 then code_cours = "INFO-L3-MIIMC-S2"
    else if i <= 2018 then code_cours = "INFO-L3-THEOIC-S2"
    else code_cours = "INFO-L3-APPREN-S2"

let lmath ~year ~firstname ~lastname d state =
  lgen "licence" ["gps2274";"gps3017";"gps2262"] dpt_maths_gps_name (Some "DMA") d ||
  List.exists
      (fun cours ->
match cours.code_cours with | None -> false | Some code_gps ->
    code_mandatory_course_DI_maths year code_gps
      &&
      match Remanent_state.get_cursus_exception
              ~firstname ~lastname ~year ~code_gps state
      with
      | _, None -> false
      | _, Some x ->
        let level = x.Public_data.class_level in
        let acronym = x.Public_data.class_dpt in
        level = "L" && acronym = "DMA"
        )    d.cours

let linfo d =
  lgen "licence" ["gps2291"] dpt_info_gps_name None d
let leco d =
  lgen "licence" ["XT01362"] dpt_eco_gps_name None d
let larts d =
  lgen "licence" ["gps69522"] dpt_arts_gps_name None d
let llila d =
  lgen "licence" ["gps83025"] dpt_lila_gps_name None d
let lpoly d =
  lgen "licence" ["gps74842"] "" None d
let lbio _ = false
let ldec _ = false

let lerasmus origine =
  match origine with
  | Some Public_data.EchErasm -> true
  | Some
      (Public_data.DensInfo
      | Public_data.DensMath
      | Public_data.DensPhys
      | Public_data.Nes
      | Public_data.AL
      | Public_data.DensDEC
      | Public_data.Info
      | Public_data.Mpi
      | Public_data.BCPST
      | Public_data.Pc
      | Public_data.PensionnaireEtranger
      | Public_data.Psi
      | Public_data.Sis
      | Public_data.ED386
      | Public_data.M_MPRI)
  | None -> false

let lechange_dri situation =
  match situation.departement_principal with
  | Some x -> Public_data.dpt_of_string x = Public_data.DRI
  | _ -> false

let lpe origine =
  match origine with
  | Some Public_data.PensionnaireEtranger -> true
  | Some
      (Public_data.DensInfo
      | Public_data.DensMath
      | Public_data.DensPhys
      | Public_data.Nes
      | Public_data.AL
      | Public_data.DensDEC
      | Public_data.EchErasm
      | Public_data.Info
      | Public_data.Mpi
      | Public_data.BCPST
      | Public_data.Pc
      | Public_data.Psi
      | Public_data.Sis
      | Public_data.ED386
      | Public_data.M_MPRI)
  | None -> false


let lmathphys d =
  (List.exists
     (fun gps_code ->
        List.exists
          (fun cours -> cours.code_cours = Some gps_code)
          d.cours)
     ["DMA-L3-B15-S2";"PHYS-L3-B11-S2"])
  ||
    if List.exists
        (fun diplome ->
          match diplome.grade with
          | None -> false
          | Some s ->
            simplify_string s = "licence")
        d.diplomes
    then
      List.exists
        (fun diplome ->
          (List.mem diplome.diplome_diplome
              [Some "gps47622";Some "gps50382";Some "gps63343";Some "gps85471";Some "gps54542"]))
        d.diplomes
      &&
      List.exists
        (fun diplome -> diplome.diplome_diplome=Some "gps3017")
        d.diplomes
    else
      d.nannee = Some 1
      &&
      (
        match d.departement_principal,d.departement_secondaire with
        | None, None -> false
        | Some _, None | None, Some _ -> false
        | Some x, Some y ->
          (simplify_string x = dpt_maths_gps_name
           && simplify_string y = dpt_phys_gps_name)
          || (simplify_string x = dpt_phys_gps_name
              && simplify_string y = dpt_maths_gps_name)
      )

let mgen dpt d =
  begin
    match
      d.departement_principal
    with
    | None -> false
    | Some x -> simplify_string x = dpt
  end

let _minfo = mgen dpt_info_gps_name
let mmaths = mgen dpt_maths_gps_name

let gen_master
    diplome' gps stage d =
  List.exists
    (fun diplome ->
       (match (Tools.map_opt String.trim diplome.niveau) with Some i ->
int_of_string i >=2 | None -> false)
       &&
       ((Tools.map_opt String.trim diplome.diplome_diplome)=Some diplome'
       ||
       List.exists
         (fun gps -> diplome.diplome_diplome=Some gps)
         gps)
    )
    d.diplomes
  ||
  List.exists
    (fun cours -> cours.code_cours = Some stage)
    d.cours

let fill_gpscodelist ~year ~firstname ~lastname list situation state =
  if lmathphys situation then
    List.rev ("gps1672"::"gps3017"::(List.rev list))
  else
  if lmath ~year ~firstname ~lastname situation state && linfo situation then
    List.rev ("gps2291"::"gps2274"::(List.rev list))
  else
    list

let mpri = gen_master "M-MPRI" ["gps62263";"gps78782"] "INFO-M2-MPRI200-S2"
let mva = gen_master "M-MVA" ["gps2228"] "INFO-M2-MVASTAGE-S2"
let iasd = gen_master "M-IASD" ["gps76822";"gps78762"] "INFO-M2-IASD-STG-S2"
let mash = gen_master "M-MASH" ["gps59622"] "INFO-M2-MASH-STG-S2"
let msesi = gen_master "M-SESI" ["gps86653"] "NOWAY"
let mint = gen_master "M-Interaction" ["gps78864"] "XT 00000000000647168"
(*let mmf = gen_master "M-MathFond" ["gps3102"] "XT 00000000000664965"*)
let mlmfi = gen_master "M-LMFI" ["gps2005";"gps3579"] "NOWAY"
let mimalis = gen_master "M-ScVivant" [] "BIO-M2-E14-S2"
let mphylo = gen_master "M-Philo" ["gps07302"] "NOWAY"
let mrandom = gen_master "M-ALEA" ["gps85775";"gps87012"] "NOWAY"
let mpropalea = gen_master "M-PROBALEA" ["gps76402"] "NOWAY"
let marianageo = gen_master "M-AAG" [] "NOWAY"
let mfondps = gen_master "M-FONDPS" ["gps85612"] "NOWAY"
let mfondsu = gen_master "M-FONDSU" ["gps87094";"gps85911";"gps86231"] "NOWAY"
let mfondupc = gen_master "M-FONDUPC" ["gps3102"] "NOWAY"
let manamodsimorsay = gen_master "M-AnaModSimOrsay" ["gps86273"] "NOWAY"
let manamodsimversailles = gen_master "M-AnaModSimVersaille" ["gps85915"] "NOWAY"
let mprobaalea = gen_master "M-ProbaAlea" ["gps86372";"gps87274"] "NOWAY"
(*let mfimfa = gen_master "M-FIMFA" ["gps3103"] "NOWAY"
let mfimfaorsay = gen_master "M-FIMFAOrsay" ["gps2458"] "NOWAY"*)
let mmod = gen_master "M-MOD" ["gps87632";"gps85959";"gps88472"] "XT 00000000000667923"
let mphys = gen_master "M-Phys" ["MPSL-PHY"] "NOWAY"
let mprobfin = gen_master "M-PROBFIN" ["gps82128"] "NOWAY"
let mformens = gen_master "M-FORMENS" ["gps87633"] "NOWAY"
let mappsu = gen_master "M-APPSU" ["gps82525"] "NOWAY"
let mape = gen_master "M-APE" ["MPSL-APE"] "NOWAY"
let agreginfo = gen_master "AGINFOSU" ["gps86919"] "NOWAY"
let agregmaths = gen_master "AGMATHUPC" ["gps85871"] "NOWAY"

let string_of_stringopt s_opt =
  match s_opt with
  | None -> ""
  | Some s -> s

let translate_dpt ~firstname ~lastname ~year state d =
  match d with
  | None ->
    Remanent_state.warn_dft
      __POS__
      (Format.sprintf "Departement non rempli pour %s %s in %i" firstname lastname year)
      Exit
      ("","")
      state
  | Some s ->
    begin
      match simplify_string s with
      | x when x=dpt_dri_gps_name -> state, (dpt_dri_full,dpt_dri_full_en)
      | x when x=dpt_info_gps_name -> state, (dpt_info_full,dpt_info_full_en)
      | x when x=dpt_maths_gps_name -> state, (dpt_maths_full,dpt_maths_full_en)
      | x when x=dpt_phys_gps_name -> state, (dpt_phys_full,dpt_phys_full_en)
      | x when x=dpt_bio_gps_name -> state, (dpt_bio_full,dpt_bio_full_en)
      | x when x=dpt_dec_gps_name        -> state, (dpt_dec_full,dpt_dec_full_en)
      | x when x=dpt_eco_gps_name -> state, (dpt_eco_full,dpt_eco_full_en)
      | x when x=dpt_arts_gps_name -> state, (dpt_arts_full,dpt_arts_full_en)
      | x when x=dpt_lila_gps_name -> state, (dpt_lila_full,dpt_lila_full_en)
      | x ->
        Remanent_state.warn
          __POS__
          (Format.sprintf "Unknown dpt %s for %s %s in %i" x firstname lastname year)
          Exit
          state,
        (Printf.sprintf
           "Département de %s"
           (Special_char.capitalize
              (Special_char.lowercase x)),
         Printf.sprintf
           "%s Department"
           (Special_char.capitalize
              (Special_char.lowercase x)))
    end

let keep_class state filter year note =
  match
    filter, Tools.map_opt Notes.valide note
  with
  | _, None ->
    Remanent_state.warn_dft
      __POS__
      "missing note when filtering classes"
      Exit
      true
      state
  | _, Some (Some true) -> state, true
  | Public_data.All, Some _
  | Public_data.All_but_in_progress, (Some None) -> state, true
  | Public_data.All_but_current_academic_year, Some _
  | Public_data.All_but_in_progress_in_current_academic_year,
    (Some None) ->
    let state, current_year =
      Remanent_state.get_current_academic_year state
    in
    state, current_year=year
  | Public_data.All_but_years l, Some _
  | Public_data.All_but_in_progress_in_years l, (Some None) ->
    state, List.mem year l
  | (Public_data.All_but_in_progress
    | Public_data.All_but_in_progress_in_current_academic_year
    | Public_data.All_but_in_progress_in_years _), Some (Some false)
    -> state, false

let keep_class
    ~firstname ~lastname ~year ~codecours ~note
    state filter =
  let state, current_year =
    Remanent_state.get_current_academic_year state
  in
  let future = year > current_year in
  let state, compensation =
      get_compensation
        ~firstname ~lastname ~year ~codecours
        note state
  in
  let state, valide = keep_class state filter year note in
  let success = valide || compensation in
  state, (not future) && success

let filter_class state filter ~firstname ~lastname ~year class_list =
  let rec aux state list acc =
    match list with
    | [] -> state, acc
    | h::t ->
      let state, codecours =
        match
          h.code_cours
        with
        | Some x -> state, x
        | None ->
          let msg =
            Printf.sprintf
              "GPS code is missing in a GPS entry (%s %s %s)" firstname lastname year
          in
          Remanent_state.warn
            __POS__
            msg
            Exit
            state, ""
      in
      let state, b =
        keep_class state filter
          ~firstname ~lastname ~year ~note:h.note ~codecours
      in
      if b then
        aux state t (h::acc)
      else
        aux state t acc
  in
  aux state class_list []

let dma_code_cours code_cours =
  Tools.substring "DMA" code_cours

let is_dma_course code_cours year =
  dma_code_cours code_cours
  ||
  begin
    try
      code_mandatory_course_DI_maths year code_cours
    with
    | _ -> false
  end
  || code_cours = "PHYS-L3-A11-S2"

let is_phys_course code_cours _year =
  Tools.substring "PHYS" code_cours

let is_di_course code_cours _year =
  Tools.substring "INFO" code_cours

let translate_diplome
    ~situation ~firstname ~lastname ~year ~code_cours
    ~origine
    state diplome =
  let code_gps = code_cours in
  match Remanent_state.get_cursus_exception
          ~firstname ~lastname ~year ~code_gps state
  with
  | state, Some x ->
    let level = x.Public_data.class_level in
    let acronym = x.Public_data.class_dpt in
    begin
      match
        Remanent_state.get_dpt
          ~acronym state
      with
      | state, Some dpt ->
        let gerund = dpt.Public_data.dpt_genitif in
        let gerund_en = dpt.Public_data.dpt_genitif_en in
        let state,label,label_en,is_m2 =
          match String.lowercase_ascii level with
          | "l" -> state, "L3 "^gerund, "Bachelor 3 "^gerund_en,false
          | "m" -> state, "M1 "^gerund, "M1 "^gerund_en,false
          | "mpri" -> state,"M2 du MPRI","M2 MPRI",true
          | "mva" -> state,"M2 du MVA","M2 MVA",true
          | "iasd" -> state,"M2 IASD","M2 IASD",true
          | "mash" -> state,"M2 MASH","M2 MASH",true
          | "sesi" -> state,"M1 SESI","M1 SESI",false
          | "agregmathsu" -> state, "Formation à l'agrégation de Mathématiques","Formation to Mathematics Aggregation",false
            | "agregmathupc" -> state, "Formation à l'agrégation de Mathématiques","Formation to Mathematics Aggregation",false
            | "agreginfosu" -> state, "Formation à l'agrégation d'Informatique","Formation to Computer Science Aggregation",false
              | "agreginfoupc" -> state, "Formation à l'agrégation d'Informatique","Formation to Computer Science Aggregation",false
          | "interaction" -> state,"M2 Interaction", "M2 Interaction", true
          | "mathfond" | "mathfondsu" | "mathfondupc" | "mathfondpantheonsor"
             -> state,"M2 Mathématiques Fondamentales", "M2 Fundamental Mathematics",true
          | "lmfi" -> state,"M2 LMFI", "M2 LMFI",true
          | "marianageo" -> state, "M2 Arithmétique Analyse et Géométrie","M2 Analysis, Number Theory and Geometry",true
          | "malea" | "alea" -> state, "M2 Mathématiques de l'Aléatoire", "M2 Mathematics of Randomness",true
          | "modsimorsay" | "modsimversailles" -> state,"M2 Mathématiques Analyse Modélisation Simulation", "M2 Mathematics Analysis Modeling Simulation",true
          | "prob" -> state,"M2 Probabilités et Modèles Aléatoires", "M2 Mathematics Probability and Random Models",true
          | "mmathgeneric" -> state,"M2 Mathématiques ?","M2 Mathematics ?",true
         | "mfimfa" | "mfimfaorsay" -> state, "M2 FIMFA", "M2 FIMFA",true
          | "mprobfin" -> state, "M2 Probabilités et Finance", "M2 Probability and Finance",true
          | "mformens" ->   state, "M2 Formation à l'Enseignement Supérieur en Mathématiques","M2 Formation to Higher Eduction in Mathematics",true
          | "mappsu" -> state,"M2 Mathematiques et applications ","M2 Mathematics and applications",true
          | "mape" -> state,"M1 Analyse politique et économique","M1 Political and economical analysis",false
          | "mmod" -> state, "M2 Mathématiques de la modelisation", "M2 Mathematics of Modeling",true
          | "mphys" -> state, "M2 Physique","M2 Physics",true
          | "imalis" -> state, "M2 IMALIS","M2 IMALIS",true
          | "philosorbonne" -> state, "M2 Phylo (SU)", "M2 Phylo (SU)", true
          | _ ->
            let msg =
              Format.sprintf
                "Unknown class level (%s)"
                level
            in
            Remanent_state.warn
              __POS__
              msg
              Exit
              state, "","", false
        in
        let dpt,dpt_en =
          match dpt.Public_data.dpt_acronyme with
          | "DI" -> "informatique","Computer Science"
          | "DMA" -> "mathématiques","Mathematics"
          | "PHYS" -> "physique","Physics"
          | "IBENS" -> "biologie","Biology"
          | "LILA" -> "littératures et langage","Litteratures and Language"
          | "ECO" -> "économie","Economy"
          | "ART" -> "arts","Arts"
          | _ -> "",""
        in
        state,
        (Some level,label,label_en,dpt,dpt_en,true,is_m2)
      | state, None ->
      let msg =
        Format.sprintf
          "Unknown department acronym (%s)"
          acronym
      in
      Remanent_state.warn_dft
        __POS__
        msg
        Exit
        (None, "","", "", "", true, false )
        state
    end
  | state, None  ->
  let check_dpt pos state origine diplome label label_en code_cours year situation is_m2 =
    match
      situation.departement_principal,
      lerasmus origine || lpe origine || lechange_dri situation
    with
    | None, false ->
      Remanent_state.warn_dft
        pos
        "Main teaching dpt is missing"
        Exit
        (Some diplome,label,label_en,"","",false,false)
        state
    | None, true ->
      state, (Some diplome,label,label_en,"","",false,false)
    | Some dpt, _  ->
      let dpt = Special_char.lowercase dpt in
      let dpt,dpt_en =
        if dpt = "mathématiques et applications"
        then
          "mathématiques","mathematics"
        else
        if dpt = "informatique"
        then
          dpt,"Computer Science"
        else
          dpt,dpt
      in
      if label = "L3" || label ="M1"
      then
        let state, (dpt,dpt_en,diplome)  =
          if dpt = "mathématiques"
          && not (is_dma_course code_cours year)
          then
            if is_di_course code_cours year
            then
              state, ("informatique","computer science","L")
            else
            if is_phys_course code_cours year
            then
              state, ("physique","physics","L")
            else
              let msg =
                Format.sprintf
                  "Cannot classify course %s (dpt:%s) for %s %s (%s)" code_cours dpt firstname lastname year
              in
              Remanent_state.warn_dft
                pos
                msg
                Exit
                (dpt,dpt_en,(if label = "L3" then "L" else label)
                 )
                state
          else
            state, (dpt, dpt_en, diplome)
        in
        let label_en =
          label^" in "^dpt_en
        in
        let label =
          if List.mem
              (String.lowercase_ascii
                 (String.sub dpt 0 1))
              ["a";"e";"i";"o";"u";"y"]
          then
            label^" d'"^dpt
          else
            label^" de "^dpt
        in
                state, (Some diplome,label,label_en,dpt,dpt_en,false,is_m2)
      else
        state, (Some diplome,label,label_en,dpt,dpt_en,false,is_m2)
  in
  match diplome with
  | Some "L" ->
    begin
      let is_m2 = false in
      if lpoly situation
      then
        check_dpt __POS__ state origine "L"
          "Bachelor de l'École Polytechnique"
          "École Polytechnique Bachelor"
          code_cours year
          situation is_m2
      else
      if lerasmus origine || lpe origine || lechange_dri situation
      then
        check_dpt __POS__ state origine "L"
          "Année d'échange"
          "Exchange year"
          code_cours year
          situation is_m2
      else
      if lmathphys situation
      then
        if is_dma_course code_cours year
        then
          state,
          (Some "L","L3 de mathématiques","Bachelor in Mathematics", dpt_maths,dpt_maths_en,false,is_m2)
        else
          state,
          (Some "L","L3 de physique","Bachelor in Physics",dpt_phys,dpt_phys_en,false,is_m2)
      else
      if linfo situation && lmath ~year ~firstname ~lastname situation state
      then
        if is_dma_course code_cours year
        then
          state,
          (Some "L","L3 de mathématiques","Bachelor in Mathematics",dpt_maths,dpt_maths_en,false,is_m2)
        else
          state,
          (Some "L","L3 d'informatique","Bachelor in Computer Science",dpt_info,dpt_info_en,false,is_m2)
      else
      if leco situation then
        state,
        (Some "L","L3 d'économie","Bachelor in Economy",dpt_eco,dpt_eco_en,false,is_m2)
      else
      if larts situation then
        state,
        (Some "L","L3 d'arts","Bachelor in Arts",dpt_arts,dpt_arts_en,false,is_m2)
      else
      if llila situation then
        state,
        (Some "L","L3 en littératures et langage","Bachelor in Litteratures and Languages",dpt_lila,dpt_lila_en,false,is_m2)
      else if ldec situation
      then
        state,
        (Some "L","L3 de sciences cognitives","Bachelor in Cognitive Sciences",dpt_dec,dpt_dec_en,false,is_m2)
      else if lbio situation then
        state,
        (Some "L","L3 de biologie","Bachelor in Biology",dpt_ibens,dpt_ibens_en,false,is_m2)
      else if ldec situation then
        state,
        (Some "L","L3 de sciences cognitives","Bachelor in Cognitive Sciences",dpt_ibens,dpt_ibens_en,false,is_m2)
      else check_dpt __POS__ state origine
        "L" "L3" "Bachelor" code_cours year
        situation is_m2

    end
  | Some "M" ->
    if mpri situation then
      state, (Some "MPRI","M2 du MPRI","M2 MPRI",dpt_info,dpt_info_en,false,true)
    else if mva situation then
      state, (Some "MVA","M2 du MVA","M2 MVA",dpt_info,dpt_info_en,false,true)
    else if iasd situation then
      state, (Some "IASD","M2 IASD","M2 IASD",dpt_info,dpt_info_en,false,true)
    else if mash situation then
      state, (Some "MASH","M2 MASH","M2 MASH", dpt_maths,dpt_maths_en,false,true)
    else if msesi situation then
      state, (Some "SESI","M1 SESI","M1 SESI", dpt_info,dpt_info_en,false,false)
    else if agregmaths situation then
      state, (Some "AGREGMATHSU","Formation à l'agrégation de Mathématiques","Formation to Mathematics Aggregation", dpt_maths, dpt_maths_en,false,false )
    else if agreginfo situation then
            state, (Some "AGREGINFOUPC","Formation à l'agrégation d'Informatique","Formation to Computer Science Aggregation", dpt_info, dpt_info_en,false,false )
    else if mint situation then
      state, (Some "Interaction", "M2 Interaction", "M2 Interaction",dpt_info,dpt_info_en,false,true)
    else if mfondsu situation then
      state, (Some "MathFondSu", "M2 Mathématiques Fondamentales", "M2 Fundamental Mathematics",dpt_maths, dpt_maths_en,false,true)
    else if mfondupc situation then
      state, (Some "MathFondUPC", "M2 Mathématiques Fondamentales", "M2 Fundamental Mathematics",dpt_maths, dpt_maths_en,false,true)
    else if mfondps situation then
      state, (Some "MathFondPantheonSor", "M2 Mathématiques Fondamentales", "M2 Fundamental Mathematics",dpt_maths, dpt_maths_en,false,true)
    else if mlmfi situation then
      state, (Some "LMFI", "M2 LMFI", "M2 LMFI", dpt_info, dpt_info_en,false,true)
    else if mrandom situation then
      state, (Some "ALEA", "M2 Mathématiques de l'Aléatoire", "M2 Mathematics of Randomness", dpt_maths, dpt_maths_en,false,true)
    else if mpropalea situation then
      state, (Some "PROBALEA", "M2 Probabilités et Modèles Aléatoires", "M2 Probabilities and Random models", dpt_maths, dpt_maths_en,false,true)
    else if marianageo situation then
      state, (Some "MARIANAGEO", "M2 Arithmétique Analyse et Géométrie","M2 Analysis, Number Theory and Geometry",dpt_maths, dpt_maths_en,false,true)
    else if manamodsimorsay situation then
      state, (Some "MODSIMORSAY", "M2 Mathématiques Analyse Modélisation Simulation", "M2 Mathematics Analysis Modeling Simulation",dpt_maths, dpt_maths_en,false,true)
    else if manamodsimversailles situation then
      state, (Some "MODSIMVERSAILLES", "M2 Mathématiques Analyse Modélisation Simulation", "M2 Mathematics Analysis Modeling Simulation",dpt_maths, dpt_maths_en,false,true)
    else if mprobaalea situation then
      state, (Some "PROB", "M2 Probabilités et Modèles Aléatoires", "M2 Mathematics Probability and Random Models",dpt_maths, dpt_maths_en,false,true)
    else if mprobfin situation then
      state, (Some "MPROBFIN", "M2 Probabilités et Finance", "M2 Probability and Finance",dpt_maths,dpt_maths_en,false,true)
    else if mformens situation then
      state, (Some "MFORMENS", "M2 Formation à l'Enseignement Supérieur en Mathématiques","M2 Formation to Higher Eduction in Mathematics",dpt_maths,dpt_maths_en,false,true)
    else if mappsu situation then
      state, (Some "M-APP", "M2 Mathematiques et applications ","M2 Mathematics and applications",dpt_maths,dpt_maths_en,false,true)
    else if mape situation then
      state, (Some "MAPE", "M1 Analyse politique et économique","M1 Political and economical analysis",dpt_maths,dpt_maths_en,false,false)
    else if mmod situation then
      state, (Some "MMOD", "M2 Mathématiques de la modelisation", "M2 Mathematics of Modeling",dpt_maths, dpt_maths_en,false,true)
    else if mphys situation then
      state, (Some "MPHYS","M2 Physique","M2 Physics",dpt_phys, dpt_phys_en,false,true)
    else if mimalis situation then
      state, (Some "IMALIS","M2 IMALIS","M2 IMALIS",dpt_bio,dpt_bio_en,false,true)
    else if mphylo situation then
      state, (Some "PHILOSorbonne","M2 Phylo (SU)", "M2 Phylo (SU)", dpt_phyl,dpt_phyl_en, false, true)
    else if agreginfo situation then
      state, (Some "AGINFOSU","Préparation à l'agrégation d'informatique","Prepartion to the Computer Science Aggregation", dpt_info,dpt_info_en,false,false)
    else if agregmaths situation then
        state, (Some "AGMATHUPC","Préparation à l'agrégation de mathématiques","Prepartion to the Mathematics Aggregation", dpt_info,dpt_info_en,false,false)
    else
    if mmaths situation then
      state, (Some "M","M1 de mathématiques","M1 in Mathematics", dpt_maths,dpt_maths_en,false,false)
    else

      check_dpt __POS__ state origine
        "M" "M1" "M1" code_cours year
        situation false
  | Some ("DENS" | "dens") ->
    state, (Some ("DENS"), "DENS", "DENS", "DENS", "DENS", false, false)
  | Some x ->
    check_dpt __POS__ state origine
      x x x code_cours year
      situation false
  | None ->
    let state, (_,b,b_en,c,c_en,d,is_m2) =
      check_dpt __POS__ state origine "" "" "" code_cours year situation false
    in
    state, (None,b,b_en,c,c_en,d,is_m2)



let color_of_dpt who pos state dpt origine =
  let dpt = simplify_string dpt in
  if dpt = dpt_info || lerasmus origine || lpe origine
  then state, Some Color.yellow
  else if dpt = dpt_maths
  then state, Some Color.orange
  else if dpt = dpt_phys
  then state, Some Color.duckblue
  else if dpt = dpt_bio
  then state, Some Color.green
  else if dpt = dpt_dec
  then state, Some Color.red
  else if dpt = dpt_eco
  then state, Some Color.pink
  else if dpt = dpt_arts
  then state, Some Color.brown
  else if dpt = dpt_lila
  then state, Some Color.blue
  else
    let msg =
      Format.sprintf "Unknown departement (%s) for %s"
        dpt
        who
    in
    Remanent_state.warn_dft
      pos
      msg
      Exit
      None
      state

let dpt_of_acro who pos state dpt origine =
  if lerasmus origine || lpe origine
  then
    state, None
  else
    match dpt with
  | Public_data.DI -> state, Some dpt_info
    | Public_data.DMA -> state, Some dpt_maths
    | Public_data.IBENS -> state, Some dpt_ibens
    | Public_data.PHYS -> state, Some dpt_phys
    | Public_data.ECO -> state, Some dpt_eco
    | Public_data.ARTS -> state, Some dpt_arts
    | Public_data.LILA -> state, Some dpt_lila
    | Public_data.DRI -> state, Some dpt_dri
    | Public_data.ENS ->
      let msg =
        Format.sprintf "Unknown departement (%s) for %s"
          (Public_data.string_of_dpt dpt) who
      in
      Remanent_state.warn_dft
        pos
        msg
        Exit
        None
        state


let stage = 250
let memoire = 240
let ecla = -10
let actd = -6
let arts = -5
let bio = -3
let dec = -2
let dma = 0
let dsa = 5
let eco = 10
let info = 20
let lila = 24
let phil = 25
let phys = 30
let vetu = 35
let autre = 40
let manquant = 50
let stage_string = "STRING"
let code_list =
  [
    stage, stage_string;
    memoire, "MIIME";
    actd, "ACTD";
    arts, "ARTS";
    bio, "BIO";
    ecla, "ECLA";
    dec, "DEC";
    dsa, "DSA";
    eco, "ECO";
    dma, "DMA";
    info, "INFO";
    lila, "LILA";
    phil, "PHIL";
    phys, "PHYS";
    vetu, "VETU";
  ]

let is_stage cours =
  begin
    match cours.code_cours with
    | None -> false
    | Some a ->
      Tools.substring "STAGE" a
      || Tools.substring "STG" a
  end
  ||
  begin
    match cours.cours_libelle with
    | None -> false
    | Some a ->
      let a = String.lowercase_ascii a in
      (Tools.substring "internship" a
       || Tools.substring "stage" a || Tools.substring "séjour linguistique" a) &&
      (not
         (Tools.substring "intensif" a))
  end

let do_report report =
  match report with
  | None -> false
  | Some b -> b

let fetch gen dft missing a =
  let _,_,_,_,cours = a in
  match cours.code_cours with
  | None -> missing
  | Some code ->
  let rec aux l =
    match l with
    | [] -> dft
    | (a,b)::t ->
      if Tools.substring b code
      then gen (a,b)
      else aux t
  in
  aux code_list

let fetch gen dft missing stage stage_string  a =
  let (_,_,_,_,cours) = a in
  if is_stage cours then
    gen (stage,stage_string)
  else
    fetch gen dft missing a

let fetch_code  = fetch snd "Hors ENS" "Sans code GPS" stage stage_string
let fetch = fetch fst autre manquant stage stage_string

let p (t,(_,_,_,_,cours)) (t',(_,_,_,_,cours')) =
  let cmp = compare t t' in
  if cmp = 0
  then compare cours.cours_libelle cours'.cours_libelle
  else cmp

let check_mandatory state cours =
  match Remanent_state.get_main_dpt state with
  | state,Public_data.DI ->
    state, if
      match cours.code_cours with
      | None -> false
      | Some a ->
        List.mem
          (String.trim a)
          [
            "INFO-L3-ALGOPRO-S1";
            "INFO-L3-LAPROCO-S1";
            "INFO-L3-SYSDIG-S1";
            "INFO-L3-LAFORMCC-S1";
            "INFO-L3-SYSRES-S2";
          ]
    then
      true
    else
      false
  | state, (Public_data.ARTS | Public_data.DRI | Public_data.ECO | Public_data.DMA | Public_data.LILA | Public_data.ENS | Public_data.IBENS | Public_data.PHYS) -> state, false

let is_mandatory state cours =
  let state, b = check_mandatory state cours in
  if b then
    state,
      (fun x -> Format.sprintf "\\mandatory{%s}" x)
  else
      state,
      (fun x -> x)

let course_by_dma cours =
  match cours.code_cours with
  | None -> false
  | Some a ->
    dma_code_cours a

let check_count_for_maths state cours =
  match Remanent_state.get_main_dpt state with
  | state,Public_data.DI ->
    state,
    begin
      match cours.code_cours with
      | None -> false
      | Some a ->
        List.mem
          (String.trim a)
          [
            "DMA-L3-A01-S1";
            "DMA-L3-A05-S2";
            "DMA-L3-A02-S1";
            "DMA-L3-A04-S1";
            "INFO-L3-APPREN-S2";
            "INFO-M2-MODGEO-S2";
            "INFO-L3-SAA-S1";
            "INFO-L3-THEOIC-S2";
          ]
        ||
        course_by_dma cours
    end
  | state, (Public_data.ARTS | Public_data.DRI | Public_data.ECO | Public_data.DMA | Public_data.LILA | Public_data.ENS | Public_data.PHYS | Public_data.IBENS) -> state, false

let count_for_maths state cours =
  let state, b = check_count_for_maths state cours in
  if b then
    state,
      (fun x -> Format.sprintf "\\countformaths{%s}" x)
  else
    state,
      (fun x -> x)

let special_course state cours =
  let state, f = is_mandatory state cours in
  let state, g = count_for_maths state cours in
  state, (fun x -> g (f x))

let get_bourse ~firstname ~lastname ~er ~current_year state =
  match Remanent_state.get_scholarship
          ~firstname ~lastname ~current_year
          state
  with
  | state, None ->
    state, ""
  | state, Some scholarship ->
    state,
    Format.sprintf " Boursi%s %s" er
      scholarship.Public_data.organism

let get_bourse_en ~firstname ~lastname ~current_year state =
  match Remanent_state.get_scholarship
          ~firstname ~lastname ~current_year
          state
  with
  | state, None ->
    state, ""
  | state, Some scholarship ->
    state,
    Format.sprintf " Scholarship %s"
      scholarship.Public_data.organism

let get_concours origin state =
  state,
  Format.sprintf
    " Concours %s"
    (string_of_origin_short_opt origin)

let get_concours_en origin state =
  state,
  Format.sprintf
    " %s track"
    (english_string_of_origin_short_opt origin)

let next_year i =
  try
    Some (string_of_int (1+int_of_string i))
  with
  | _ -> None

let mean_init = (StringOptMap.empty,[])
let cours_list_init=Public_data.empty_repartition_diplomes
let stage_list_init=[]
let m2_init = []
let dip_autre_list_init = []
let dens_init = Public_data.YearMap.empty
let n_att_init = Public_data.YearMap.empty

let translate_course_dens course =
{
 Public_data.supplement_code=(match course.code_cours with None -> "" | Some c -> c);
 Public_data.supplement_discipline="";
 Public_data.supplement_intitule="";
 Public_data.supplement_ects=(match course.ects with None -> 0. | Some i -> i) ;
 Public_data.supplement_dens=true
}


let translate_course_nat course =
{(translate_course_dens course) with
 Public_data.supplement_dens=false
}

let add_dens_requirements state year course map =
  match course.ects with
  | None -> state, map
  | Some _ ->
    let total,potential,mandatory,math,math_math_info =
      match
        Public_data.YearMap.find_opt year map
      with
      | None -> (0.,0.,0,0,0)
      | Some a -> a
    in
    let state, is_mandatory = check_mandatory state course in
    let mandatory =
      if is_mandatory
      then
        mandatory+1
      else
        mandatory
    in
    let state, count_for_maths = check_count_for_maths state course in
    let math_math_info =
      if count_for_maths
      then
        math_math_info + 1
      else
        math_math_info
    in
    let course_by_dma = course_by_dma course in
    let math =
      if course_by_dma
      then
        math+1
      else
        math
    in
    state, Public_data.YearMap.add year
      (total, potential,mandatory,math,math_math_info) map


let add_dens_ok state year course course_list map =
  match course.ects with
  | None -> state,
            {course_list with Public_data.dens  = (translate_course_dens course)::course_list.Public_data.dens},
            map
  | Some f ->
    let total,potential,mandatory,math,math_math_info =
      match
        Public_data.YearMap.find_opt year map
      with
      | None -> (0.,0.,0,0,0)
      | Some a -> a
    in
    let total = total+.f in
    let map =
      Public_data.YearMap.add year
        (total, potential,mandatory,math,math_math_info) map
    in
    let state, map = add_dens_requirements state year course map in
    state,
    {course_list with Public_data.dens  = (translate_course_dens course)::course_list.Public_data.dens}, map

let add_dens_potential year course map =
  match course.ects with
  | None -> map
  | Some f ->
    let total,potential,mandatory,math,math_math_info =
      match
        Public_data.YearMap.find_opt year map
      with
      | None -> (0.,0.,0,0,0)
      | Some a -> a
    in
    Public_data.YearMap.add year
      (total, potential+.f,mandatory,math,math_math_info)
      map

let add_dens state year compensation course course_list map =
  match compensation, course.note with
  | Some _, _ -> add_dens_ok state year course course_list map
  | None,None -> state, course_list, add_dens_potential year course map
  | None,Some note ->
      match
        Notes.valide note
      with
      | Some true -> add_dens_ok state year course course_list map
      | Some false -> state, course_list, map
      | None -> state, course_list, add_dens_potential year course map

let add_mean_empty is_m2 state ~dens ~natt ~decision ~exception_cursus key year  map =
  let is_m2,ects,old,y =
    match StringOptMap.find_opt key (fst map)
    with
    | None -> is_m2,0.,[],0
    | Some a -> a
  in
  if ects < 60. || exception_cursus || decision || dens || natt
  then
    state,
    (StringOptMap.add key (is_m2,ects,old, max y year) (fst map),
     snd map)
  else
    state, map

let add_mean_ok is_m2 state key course course_list year map dens =
  let is_m2,ects,old,y =
    match StringOptMap.find_opt key (fst map)
    with
    | None -> is_m2,0.,[],0
    | Some a -> a
  in
  let year_int =
    try int_of_string year with _ -> 0
  in
  let map =
    StringOptMap.add key
            (is_m2,ects+.(match course.note
                    with
                    | None -> 0.
                    | Some a ->
                      begin
                        match  Notes.valide a with
                        | None | Some false -> 0.
                        | Some true ->
                          begin
                            match course.ects
                            with
                            |  None -> 0.
                            |  Some a -> a
                          end
                      end
                   )
            ,(course.note, course.ects)::old, max y year_int) (fst map),
    snd map
  in
  let state, dens =
    add_dens_requirements state year course dens
  in
  let course_list =
      {course_list with Public_data.diplomes_nationaux = (translate_course_nat course)::course_list.Public_data.diplomes_nationaux }
  in
  state, map, course_list, dens

let add_mean is_m2 state key compensation course course_list year map dens =
    match compensation, course.note with
  | Some _, _ -> add_mean_ok is_m2 state key course course_list year map dens
  | None,None -> state, map, course_list, dens
  | None,Some note ->
      match
        Notes.valide note
      with
      | Some true -> add_mean_ok is_m2 state key course course_list year map dens
      | Some false | None ->
        state, map, course_list, dens

let add_mean_diplome is_m2 state ~dens ~natt ~decision ~exception_cursus d mean_opt mention_opt validated_opt year mean =
  add_mean_empty is_m2 state ~dens ~natt ~decision ~exception_cursus
    d year (fst mean, (d,mean_opt,mention_opt,validated_opt,year)::(snd mean))

let get_origine who promo gps_file state =
  match
    gps_file.origine
  with
  |(Some
      ( Public_data.DensInfo
      | Public_data.DensDEC
      | Public_data.DensMath
      | Public_data.DensPhys
      | Public_data.Nes
      |Public_data.EchErasm
      |Public_data.Info
      |Public_data.Mpi
      |Public_data.BCPST
      |Public_data.Pc
      |Public_data.PensionnaireEtranger
      |Public_data.Psi
      |Public_data.Sis
       | Public_data.AL)
   | None) as x -> state, x
  | Some Public_data.ED386
  | Some Public_data.M_MPRI ->
    begin
      match
        Public_data.YearMap.find_opt
          promo
          gps_file.situation
      with
      | None ->
        Remanent_state.warn
          __POS__
          (Format.sprintf
             "cannot find situation in %s (%s)"
             promo who)
          Exit
          state, None
      | Some situation ->
        let rec aux l state =
          match l with
          | [] -> state, None
          | h::t ->
            if List.mem h.grade
                [Some "Concours d'entrée ENS";
                 Some "Sélection internationnale"]
            then
              origin_opt_of_concours
                ~who __POS__ state
                h.diplome_diplome
            else
              aux t state
        in aux situation.diplomes state
    end

let is_elligble_for_funding origine gps_file state =
  match gps_file.statut with
  | None
  | Some (
      Public_data.Ex_boursier_si
    | Public_data.Boursier_si
    | Public_data.Ex_eleve
    | Public_data.Ex_eleve_bis
    | Public_data.Eleve_bis
    | Public_data.Eleve ) -> state, false
  | Some (Public_data.Ex_etudiant
         | Public_data.Etudiant) -> state, true
  | Some
      ( Public_data.Hors_GPS
      | Public_data.Ex_hors_GPS)  ->
    begin
      match origine with
      | None
      | Some
          (
            Public_data.PensionnaireEtranger
          | Public_data.EchErasm
          | Public_data.M_MPRI
          | Public_data.ED386
          | Public_data.Info
          | Public_data.Mpi
          | Public_data.BCPST
          | Public_data.Pc
          | Public_data.Psi
          | Public_data.AL
          | Public_data.Sis) ->
        state, false
      | Some ( Public_data.Nes
             | Public_data.DensMath
             | Public_data.DensPhys
             | Public_data.DensInfo
             | Public_data.DensDEC)
        -> state, true
    end

let not_dispense ~firstname ~lastname ~year state =
    match Remanent_state.get_dispenses ~firstname ~lastname ~year state with
      | _, []-> true
      | _, _::_ -> false

let heading
    ?dens ~who ~firstname ~lastname ~promo ~origine
    ~year ~situation ~gpscodelist ~tuteur ?tuteur_bis
    cursus_map split_cours picture_list is_suite gps_file state =
  let state, main_dpt =
    Remanent_state.get_main_dpt state
  in
  let genre,er,_ne =
    match gps_file.genre with
    | None | Some Public_data.Unknown -> "(e)","er(\\`ere)","(ne)"
    | Some Public_data.Masculin -> "","er",""
    | Some Public_data.Feminin -> "e","\\`ere","ne"
  in
  let backgroundcolor = Some Color.green in
  let state =
    match Remanent_state.get_main_dpt state with
    | state, Public_data.DI ->
      let () =
        Remanent_state.log_string
          ?backgroundcolor
          state
          ~english:"Department of Computer Science. \\'Ecole Normale Sup\\'erieure. 45, rue d'Ulm 75005 Paris. Phone: +33 (0)1 44 32 20 45."
          "D\\'epartement d'Informatique.  \\'Ecole  Normale  Sup\\'erieure. 45, rue d'Ulm 75005 Paris. Tel : +33 (0)1 44 32 20 45."
      in state
    | state, Public_data.DMA ->
      let () =
        Remanent_state.log_string
          ?backgroundcolor
          state
          ~english:"Department of Mathematics and their Applications. \\'Ecole Normale Sup\\'erieure. 45, rue d'Ulm 75005 Paris. Phone: +33 (0)1 44 32 20 49."
          "D\\'epartement de Math\\'ematiques et Applications. \\'Ecole  Normale  Sup\\'erieure. 45, rue d'Ulm 75005 Paris. Tel : +33 (0)1 44 32 20 49."
      in
      state
      | state, Public_data.PHYS ->
        let () =
          Remanent_state.log_string
            ?backgroundcolor
            state
            ~english:"Department of Physics. \\'Ecole Normale Sup\\'erieure. XXXXXXXXXXX 75005 Paris. Phone: +33 (0)1 44 32 ?? ??."
            "D\\'epartement de Physique. \\'Ecole  Normale  Sup\\'erieure. 4XXXXXXX 75005 Paris. Tel : +33 (0)1 44 32 ?? ??."
        in
        state
      | state, (Public_data.ARTS | Public_data.DRI | Public_data.ENS | Public_data.ECO | Public_data.IBENS | Public_data.LILA) ->
      let state =
        Remanent_state.warn
          __POS__
          "ARTS/DRI/ENS/IBENS/ECO are not a valid dpt to edit transcripts"
          Exit
          state
      in state
  in
  let () =
    Remanent_state.print_newline state in
  let backgroundcolor = Some Color.blue in
  let lineproportion = Some (2./.3.) in
  let current_year = year in
  let state, bourse, bourse_en =
    let state, b = is_elligble_for_funding origine gps_file state in
    if b
    then
      let state, bourse =
        get_bourse
          ~firstname ~lastname ~er ~current_year state
      in
      let state, bourse_en =
        get_bourse_en
          ~firstname ~lastname ~current_year state
      in
      state, bourse, bourse_en
    else state, "", ""
  in
  let state,statut,statut_en,concours,concours_en =
    match gps_file.statut with
    | None -> state,"","","",""
    | Some
        (Public_data.Ex_boursier_si
        | Public_data.Boursier_si) ->
      state,
      Format.sprintf "\\'Etudiant%s SI" genre,
      "Student (Int. Sel.)",
      "",""
    | Some
        (Public_data.Ex_eleve
        | Public_data.Ex_eleve_bis
        | Public_data.Eleve_bis
        | Public_data.Eleve) ->
      let state, concours =
        get_concours origine state
      in
      let state, concours_en =
        get_concours_en origine state
      in
      state,"\\'El\\`eve","Student",concours,concours_en
    | Some (Public_data.Ex_etudiant
           | Public_data.Etudiant )->
      begin

        state,Format.sprintf "\\'Etudiant%s" genre,
        "Student","",""
      end
    | Some
        ( Public_data.Hors_GPS
        | Public_data.Ex_hors_GPS)  ->
      begin
        match origine with
        | Some Public_data.PensionnaireEtranger ->
          state,"Pensionnaire \\'Etranger","Int. Exchange","",""
        | Some Public_data.EchErasm ->
          state,"\\'Echange Erasmus","Erasmus student","",""
        | Some Public_data.M_MPRI ->
          Remanent_state.warn
            __POS__
            (Format.sprintf "Illegal origin (M-MPRI) for %s" who)
            Exit
            state,
          "M-MPRI","M-MPRI","",""
        | None
        | Some Public_data.ED386
          -> state, "","", "",""
        | Some ( Public_data.Nes
               | Public_data.DensMath
               | Public_data.DensPhys
               | Public_data.DensInfo
               | Public_data.DensDEC)
          ->
            state,
          Format.sprintf
            "\\'Etudiant%s"
            genre,"Student","",""
        | Some Public_data.Info
        | Some Public_data.Mpi
        | Some Public_data.BCPST
        | Some Public_data.Pc
        | Some Public_data.Psi
        | Some Public_data.AL
          ->
          let state, concours =
            get_concours origine state
          in
          let state, concours_en =
            get_concours_en origine state
          in
          state,
          "\\'El\\`eve","Student",
          concours,concours_en
        | Some Public_data.Sis ->
          state,
          Format.sprintf "\\'Etudiant%s SI" genre,"Student (Int. Sel)","",""
      end
  in
  let () =
    Remanent_state.log_string
      ?backgroundcolor
      ?lineproportion
      state
      ~english:(Format.sprintf
         "\\large %s %s \\hspace{5mm} born on %s \\hspace{5mm} %s%s%s"
         (Special_char.uppercase
            (Tools.unsome_string
               gps_file.nom))
         (Special_char.capitalize
            (Tools.unsome_string
               gps_file.prenom))
         (Tools.date_to_string_en (Tools.unsome_string gps_file.date_de_naissance))
         statut_en
         bourse_en
         concours_en)
      (Format.sprintf
      "\\large %s %s \\hspace{5mm} n\\'e%s le %s \\hspace{5mm} %s%s%s"
      (Special_char.uppercase
         (Tools.unsome_string
            gps_file.nom))
      (Special_char.capitalize
         (Tools.unsome_string
            gps_file.prenom))
      genre
      (Tools.unsome_string gps_file.date_de_naissance)
      statut
      bourse
      concours)
  in
  let lineproportion = Some (1./.3.) in
  let () =
    Remanent_state.log_string
      ?backgroundcolor
      ?lineproportion
      state
      ~english:(Format.sprintf "\\large Promotion: %s" promo)
      (Format.sprintf "\\large Promotion : %s"
         promo)
  in
  let () =
    Remanent_state.print_newline state
  in
  let tuteur, tuteur_en, lineproportion = tuteur in
  let backgroundcolor =
    match
      dens, situation.nannee
    with
    | Some true, _ -> Color.blue
    | (None | Some false), None -> Color.orange
    | (None | Some false), Some _ -> Color.yellow
  in
  let textcolor = Color.red in
  let state, annee, annee_int =
    match dens with
    | Some true -> state, "", 0
    | None | Some false ->
      let state, annee_int =
        try
          state, int_of_string year
        with
          | _ ->
            let msg =
              Format.sprintf
              "Ill-formed year %s"
              year
            in
            Remanent_state.warn
              __POS__
              msg
              Exit
              state,
              0
      in
      let annee =
      Printf.sprintf
          "%i -- %i" annee_int (annee_int+1)
      in
      state, annee, annee_int
  in
  let state, statut, statut_en, nationaux_opt, nationaux_en_opt =
    match dens with Some true -> state, "DENS","DENS",None,None
                | None | Some false ->
    if lerasmus origine
    || lpe origine
    then
      state,
      (Format.sprintf "Année d'étude au département %s"
         (match main_dpt with
            Public_data.DI -> "d'informatique"
          | Public_data.DMA -> "de mathématiques"
          | Public_data.ENS -> ""
          | Public_data.PHYS -> "de physique"
          | Public_data.IBENS -> "de biologie"
          | Public_data.ECO -> "d'économie"
          | Public_data.ARTS -> "d'arts"
          | Public_data.LILA -> "de littératures et langage"
          | Public_data.DRI -> "")
      ),
      (Format.sprintf "Year: %s Department"
         (match main_dpt with
            Public_data.DI -> "Computer Science"
          | Public_data.DMA -> "Mathematics"
          | Public_data.ENS -> ""
          | Public_data.PHYS -> "Physics"
          | Public_data.IBENS -> "Biology"
          | Public_data.ECO -> "Economy"
          | Public_data.ARTS -> "Arts"
          | Public_data.LILA -> "Litteratures and Language"
          | Public_data.DRI -> "")
         ),
      None, None
    else if lechange_dri situation then
      state,
      "Année d'échanges avec la direction des relations internationales",
      "Exchange Year with the International Relations Office",
      None, None
    else
      match
        situation.nannee
      with
      | None ->
        state, "Césure", "Study Break",None, None
      | Some i ->
        begin
          let is_suite_fr, is_suite_en =
            if is_suite
            then "(suite) "," (continued)"
            else "",""
          in
          let state, (prefix_fr, prefix_en) =
            match i with
            | 1 -> state,
                   (Format.sprintf "Première année %s:" is_suite_fr,
                    Format.sprintf "First Year%s:" is_suite_en)
            | 2 -> state,
                   (Format.sprintf "Seconde année %s :" is_suite_fr,
                    Format.sprintf "Second Year%s:" is_suite_en)
            | 3 -> state,
                   (Format.sprintf "Troisième année %s :" is_suite_fr,
                    Format.sprintf "Third Year%s:" is_suite_en)
            | 4 -> state,
                   (Format.sprintf "Quatrième année %s:" is_suite_fr,
                    Format.sprintf "Fourth Year%s:" is_suite_en)
            | 5 -> state,
                   (Format.sprintf "Cinquième année %s:" is_suite_fr,
                    Format.sprintf "Fifth Year%s:" is_suite_en)
            | 6 -> state,
                   (Format.sprintf "Sixième année %s:" is_suite_fr,
                    Format.sprintf "Sixth Year%s:" is_suite_en)
            | _ ->
              let msg =
                Printf.sprintf
                  "max 6 ans de scolarité pour %s"
                  who
              in
              Remanent_state.warn_dft
                __POS__
                msg
                Exit
                ((string_of_int i)^"ème année "^is_suite_fr^":",
                 (string_of_int i)^"th Year"^is_suite_en^":")
                state
          in
          let state, suffix_fr, suffix_en, nationaux_opt, nationaux_en_opt
            =
            if
              lmath ~year ~firstname ~lastname situation state
              &&
              linfo situation
              &&
              not_dispense ~firstname ~lastname ~year state
            then
              let state, dpt =
                match
                  situation.departement_principal
                with
                | Some x ->
                  let s  = simplify_string x in
                  if s = dpt_info_gps_name
                  then
                    state,acro_dpt_info
                  else
                  if s = dpt_maths_gps_name
                  then state,
                       acro_dpt_maths
                  else
                    let msg =
                      Printf.sprintf
                        "mauvais dpt principal pour une double licence pour %s"
                        who
                    in
                    Remanent_state.warn_dft
                      __POS__
                      msg
                      Exit
                      "DI"
                      state
                | _ ->
                  let msg =
                    Printf.sprintf
                      "mauvais dpt principal pour une double licence maths-info pour %s"
                      who
                  in
                  Remanent_state.warn_dft
                    __POS__
                    msg
                    Exit
                    "DI"
                    state
              in
              if annee_int < 2020 then
                state,
                Printf.sprintf
                  "Cursus maths-info et rattaché%s au %s"
                  genre dpt,
                Printf.sprintf
                  "Maths-CS program, registered at %s" dpt,
                Some
                  "Licence L3 Info et L3 Maths Université Paris Diderot",
                Some
                  "Bachelor in Computer Science and Bachelor in Maths at Paris-Diderot University"
              else if annee_int=2020 then
              state,
              Printf.sprintf
                "Cursus maths-info et rattaché%s au %s"
                genre dpt,
              Printf.sprintf
                "Maths-CS program, registered at %s" dpt,
              Some
                "Licence L3 Info et L3 Maths Université de Paris",
              Some
                "Bachelor in Computer Science and Bachelor in Maths at University of Paris"
              else
              state,
              Printf.sprintf
                "Cursus maths-info et rattaché%s au %s"
                genre dpt,
              Printf.sprintf
                "Maths-CS program, registered at %s" dpt,
              Some
                "Licence L3 Info et L3 Maths Université de Paris Cité",
              Some
                "Bachelor in Computer Science and Bachelor in Maths at University of Paris City"
            else if
              lmathphys situation
              && not_dispense ~firstname ~lastname ~year state
            then
              let state, dpt =
                match
                  situation.departement_principal
                with
                | Some x ->
                  let s  = simplify_string x in
                  if s = dpt_phys_gps_name
                  then
                    state,acro_dpt_phys
                  else if s =
                          dpt_maths_gps_name
                  then state, acro_dpt_maths
                  else if s =
                          dpt_info_gps_name
                  then state, acro_dpt_info
                  else
                    let msg =
                      Printf.sprintf
                        "mauvais dpt principal pour une double licence maths-physique pour %s"
                        who
                    in
                    Remanent_state.warn_dft
                      __POS__
                      msg
                      Exit
                      "DI"
                      state
                | _ ->
                  let msg =
                    Printf.sprintf
                      "mauvais dpt principal pour une double licence pour %s"
                      who
                  in
                  Remanent_state.warn_dft
                    __POS__
                    msg
                    Exit
                    "DI"
                    state
              in
              if annee_int < 2020 then
                state,
                Printf.sprintf
                  "Cursus maths-physique et rattaché au %s" dpt,
                Printf.sprintf
                  "Maths-Phys program, registed at %s " dpt,
                Some "Licence L3 Maths et L3 Phys Université Paris-Sud",
                Some "Bachelor in Maths and Bachelor in Physics at Paris-South  University"
              else
              state,
              Printf.sprintf
                "Cursus maths-physique et rattaché au %s" dpt,
              Printf.sprintf
                "Maths-Phys program, registed at %s " dpt,
              Some "Licence L3 Maths et L3 Phys Université Paris-Saclay",
              Some "Bachelor in Maths and Bachelor in Physics at Paris-Saclay   University"
            else
              let state, (string,string_en) =
                translate_dpt ~firstname ~lastname ~year:annee_int state
                  situation.departement_principal
              in
              state, string, string_en, None, None
          in
          state,
          Printf.sprintf
            "%s %s" prefix_fr suffix_fr,
          Printf.sprintf
            "%s %s" prefix_en suffix_en,
          nationaux_opt,
          nationaux_en_opt
        end
  in
  let statut, statut_en =
        match dens with
            | Some true -> "BILAN DENS","DENS SUMMARY"
            | None | Some false -> statut, statut_en
  in
    let state, dens_opt, dens_en_opt  =
    match
      situation.inscription_au_DENS
    with
    | Some true ->
      begin
        match nationaux_opt with
        | Some _ -> state, ["Diplôme de l'ENS"], ["ENS diploma"]
        | _ ->
          let state, cursus_opt =
            if lmath ~year ~firstname ~lastname situation state
            || lmathphys situation
            then
              Remanent_state.get_cursus
                ~year
                ~level:"dens"
                ~gpscodelist:[]
                __POS__
                state
            else
              Remanent_state.get_cursus
                ~year
                ~level:"dens"
                ~gpscodelist:[]
                ~dpt:main_dpt
                __POS__
                state
          in
          match cursus_opt with
          | Some cursus ->
            begin
              match cursus.Public_data.inscription, cursus.Public_data.inscription_en
              with
              | None, None -> state, ["DENS"],["DENS"]
              | Some x, Some y -> state, [x], [y]
              | Some x, None | None, Some x ->
                let state =
                  Remanent_state.warn
                    __POS__
                    (Format.sprintf "The translation of %s is missing (cursus registration for %s)" x cursus.Public_data.cursus_niveau)
                    Exit state
                in
                state, [x], [x]
            end
          | None ->
            let msg =
              Format.sprintf
                "Undocumented cursus dens %s"
                year
            in
            Remanent_state.warn
              __POS__
              msg
              Exit
              state,
            [],[]
      end
    | Some false
    | None -> state, [], []
  in
  let state, inscriptions, inscriptions_en, inscriptions_short, inscriptions_en_short, is_l3 =
    match nationaux_opt,nationaux_en_opt with
    | Some x,Some y -> state, x::dens_opt, y::dens_en_opt, x::dens_opt, y::dens_en_opt, true
    | Some x,None | None, Some x ->
      let state =
        Remanent_state.warn
          __POS__
          (Format.sprintf "The translation of %s is missing" x)
          Exit
          state
      in
      state, x::dens_opt, x::dens_en_opt, x::dens_opt, x::dens_en_opt, true
    | None, None ->
      if lpoly situation
      then
        state, "Bachelor de l'X"::dens_opt, "X Bachelor"::dens_en_opt, "Bachelor de l'X"::dens_opt, "X Bachelor"::dens_en_opt, true
      else if
        lpe origine
        || lerasmus origine
      then
        state, dens_opt, dens_en_opt, dens_opt, dens_en_opt, false
      else
        StringOptMap.fold
          (fun (string_opt,dpt) _
            (state,inscriptions,inscriptions_en, inscriptions_short, inscriptions_en_short, is_l3) ->
            match string_opt with
            | None | Some "dens" | Some "autre" ->
              state, inscriptions, inscriptions_en, inscriptions_short, inscriptions_en_short, is_l3
            | Some string ->
              match
                StringOptMap.find_opt
                  (string_opt,dpt)
                  cursus_map
              with
              | None ->
                let state =
                  Remanent_state.warn
                    __POS__
                    (Printf.sprintf
                       "internal error, cursus should be stored in cursus_map %s %s %s"
                       (Tools.unsome_string string_opt)
                       dpt
                       who)
                    Exit
                    state
                in
                let state =
                  StringOptMap.fold
                    (fun (string_opt,dpt) _ state ->
                       Remanent_state.warn
                         __POS__
                         (Format.sprintf
                            "-> %s %s"
                            (Tools.unsome_string string_opt)
                            dpt)
                         Exit
                         state)
                    cursus_map
                    state
                in
                state,
                inscriptions,
                inscriptions_en,
                inscriptions_short,
                inscriptions_en_short,
                is_l3
              | Some (debut,fin) ->
                try
                  if
                    String.trim string <> ""
                    &&
                    begin
                      match debut with
                      | None -> false
                      | Some debut ->
                        int_of_string debut <=
                        int_of_string year
                    end
                    &&
                    match fin with
                    | None -> false
                    | Some fin ->
                      int_of_string year <= int_of_string
                        fin
                  then
                      let state, cursus_opt =
                        Remanent_state.get_cursus
                          __POS__
                          ~year
                          ~dpt:(Public_data.dpt_of_string dpt)
                          ~gpscodelist
                          ~firstname ~lastname
                          ~level:string
                          state
                      in
                      match cursus_opt with
                    | None ->
                      let msg =
                        Format.sprintf
                          "Undocumented cursus %s %s %s"
                          string
                          dpt
                          year
                      in
                      Remanent_state.warn
                        __POS__
                        msg
                        Exit
                        state,
                      inscriptions,
                      inscriptions_en,
                      inscriptions_short,
                      inscriptions_en_short,
                      is_l3
                    | Some cursus ->
                      let short =
                        cursus.Public_data.cursus_gps = None &&
                        cursus.Public_data.cursus_niveau = "m" &&
                        situation.nannee = Some 1
                      in
                      let is_l3 =
                        is_l3 || (cursus.Public_data.cursus_niveau = "l" &&
                        situation.nannee = Some 1)
                      in
                      match
                          cursus.Public_data.inscription, cursus.Public_data.inscription_en,
                          cursus.Public_data.cursus_univ
                        with
                        | Some inscription, Some inscription_en, Some univ ->
                          let inscription =
                              Format.sprintf
                                "%s --- %s"
                                inscription
                                (Public_data.string_of_universite_long_fr univ)
                        in
                        let inscription_en =
                            Format.sprintf
                                "%s --- %s"
                                inscription_en
                                (Public_data.string_of_universite_long_en univ)
                      in
                      state,
                        inscription::inscriptions,
                        inscription_en::inscriptions_en,
                        (if short then inscriptions_short else  inscription::inscriptions_short),
                        (if short then inscriptions_en_short else inscription_en::inscriptions_en_short),
                        is_l3
                      | Some x, None,Some univ  | None, Some x, Some univ ->
                      let state =
                        Remanent_state.warn
                          __POS__
                          (Format.sprintf "The translation of %s is missing (cursus registration for %s)" x cursus.Public_data.cursus_niveau)
                          Exit
                          state
                      in
                      let inscription, inscription_en = x,x in
                      let inscription =
                          Format.sprintf
                              "%s --- %s"
                              inscription
                              (Public_data.string_of_universite_long_fr univ)
                      in
                      let inscription_en =
                          Format.sprintf
                              "%s --- %s"
                              inscription_en
                              (Public_data.string_of_universite_long_en univ)
                    in

                      state, inscription::inscriptions,
                      inscription_en::inscriptions_en,
                      (if short then inscriptions_short else inscription::inscriptions_short),
                      (if short then inscriptions_en_short else   inscription_en::inscriptions_en_short),
                        is_l3
                      | None, None, _   ->
                        let msg =
                          Format.sprintf
                            "Inscription field is not documented for cursus %s %s %s"
                            string
                            dpt
                            year
                        in
                        Remanent_state.warn
                          __POS__
                          msg
                          Exit
                          state,
                        inscriptions, inscriptions_en, inscriptions_short, inscriptions_en_short, is_l3
                        | _, _, None   ->
                          let msg =
                            Format.sprintf
                              "University field is not documented for cursus %s %s %s"
                              string
                              dpt
                              year
                          in
                          Remanent_state.warn
                            __POS__
                            msg
                            Exit
                            state,
                          inscriptions, inscriptions_en, inscriptions_short,
                          inscriptions_en_short, is_l3
                  else state, inscriptions, inscriptions_en,   inscriptions_short,
                  inscriptions_en_short,
                  is_l3
                with _ ->
                  Remanent_state.warn
                    __POS__
                    "internal error, years should be  convertible into int"
                    Exit
                    state,
                  inscriptions, inscriptions_en, inscriptions_short,
                  inscriptions_en_short,
                  is_l3
          )
          split_cours
          (state, dens_opt, dens_en_opt, dens_opt, dens_en_opt, false)
  in
  let inscriptions, inscriptions_en =
      if is_l3 then
         inscriptions_short, inscriptions_en_short
      else
         inscriptions, inscriptions_en
  in
  let inscription_string =
    Format.asprintf
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun log _ ->
             Format.fprintf log " + ")
         (fun log -> Format.fprintf log "%s")
      )
      inscriptions
  in
  let inscription_en_string =
    Format.asprintf
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun log _ ->
             Format.fprintf log " + ")
         (fun log -> Format.fprintf log "%s")
      )
      inscriptions_en
  in
  let () =
    Remanent_state.log_string
      ~lineproportion
      ~backgroundcolor
      ~textcolor
      state
      ~english:(Format.sprintf "%s %s "
                  annee
                  statut_en)
      (Format.sprintf "%s %s "
         annee
         statut)

  in
  let () =
    if not (tuteur = "") then
      let lineproportion = 1./.3. in
      Remanent_state.log_string
        ~lineproportion
        ~backgroundcolor
        ~textcolor
        ~english:tuteur_en
        state
        tuteur
  in
  let () =
    Remanent_state.print_newline state
  in
  let f x =
    Printf.sprintf
      "\\vspace*{-2cm}{\\hfill\\includegraphics[height=2cm]{%s}}\\mbox{}"
      x
  in
  let state, s  =
    Tools.include_latex_list
      f
      state
      picture_list
  in
  let () =
    Remanent_state.fprintf_verbatim
      state
      "%s"
      s
  in
  let () =
    Remanent_state.print_newline state
  in
  let state, list =
    Remanent_state.get_dispenses
      ~firstname ~lastname ~year
      state
  in
  let lineproportion = 1. in
  let textcolor = Color.red in
  let backgroundcolor = Color.yellow in
  let state =
    List.fold_left
      (fun state dispense ->
         match dispense.Public_data.dispense_motif,
               dispense.Public_data.dispense_motif_en
         with
         | None, None ->
           let msg =
             Printf.sprintf
               "dispense without explaination for %s %s in %s"
               firstname lastname year
           in
           Remanent_state.warn
             __POS__
             msg
             Exit
             state
         | Some motif, None | None, Some motif ->
         let msg =
           Printf.sprintf
             "The translation of an explaination for a dispense is missing for %s %s in %s (%s)"
             firstname lastname year motif
         in
         let state =
           Remanent_state.warn
             __POS__
             msg
             Exit
             state
         in
         let () =
           Remanent_state.log
             ~lineproportion
             ~backgroundcolor
             ~textcolor
             state
             "%s"
             motif
         in
         let () =
           Remanent_state.print_newline state
         in
         state
         | Some motif, Some motif_en ->
           let () =
             Remanent_state.log_string
               ~lineproportion
               ~backgroundcolor
               ~textcolor
               ~english:motif_en
               state
               motif
           in
           let () =
             Remanent_state.print_newline state
           in
           state
      ) state list
  in
  let () =
    Remanent_state.print_newline state
  in
  let () =
    match inscription_string, tuteur_bis with
    | "", None  -> ()
    | "", Some (tuteur,tuteur_en,lineproportion) ->
      let () =
        Remanent_state.log
          ~lineproportion
          state
          " "
      in
      let lineproportion = 1./.3. in
      let () =
        Remanent_state.log_string
          ~lineproportion
          ~english:tuteur_en
          state
          tuteur
      in
      let () =
        Remanent_state.print_newline state
      in
      ()
    | _, None ->
      let lineproportion = 1. in
      let () =
        Remanent_state.log_string
          ~lineproportion
          state
          ~english:(Format.sprintf "Registrations: %s" inscription_en_string)
          (Format.sprintf "Inscriptions : %s"
             inscription_string)
      in
      let () =
        Remanent_state.print_newline state
      in
      ()
    | _, Some (tuteur,tuteur_en,lineproportion) ->
      let () =
        Remanent_state.log
          ~lineproportion
          state
          "Inscriptions : %s"
          inscription_string
      in
      let lineproportion = 1./.3. in
      let () =
        Remanent_state.log_string
          ~lineproportion
          ~english:tuteur_en
          state
          tuteur
      in
      let () =
        Remanent_state.print_newline state
      in
      ()
  in state, is_l3

let foot signature state  =
  let () =
    match signature with
    | [] -> ()
    | sign_list ->
      let state, article =
        Remanent_state.bilingual_string ~english:"the" ~french:"le" state
      in
      let () =
        Remanent_state.fprintf
          state
          "\\vfill\n\n\\vspace*{-1.cm}\n\n\\begin{center}%%\n\ Paris, %s \\today\\\\%%\n\ " article
      in
      let f x =
        Printf.sprintf
          "\\includegraphics[height=2cm]{%s}\\hspace*{5mm}\\mbox{}"
          x
      in
      let state, s =
        Tools.include_latex_list
          f
          state
          sign_list
      in
      let () =
        Remanent_state.fprintf_verbatim
          state
          "%s"
          s
      in
      let () =
        Remanent_state.fprintf
          state
          "\\end{center}\\vfill%%\n\ "
      in
      let () =
        Remanent_state.print_newline state
      in
      let () =
        Remanent_state.breakpage state
      in
      ()
  in
  state

let program
    ~print_foot_note
    ~origine ~gpscodelist ~string ~dpt ~year ~who ~alloc_suffix ~mean ~cours_list ~stage_list ~firstname ~lastname ~promo ~cursus_map
    ~size ~stages ~current_year (*~report ~keep_faillure ~keep_success*)
    ~dens ~natt ~is_m2
    (list:(bool * string * string * string * cours) list) state =
  let state,
      entete,entete_en,
      footpage,footpage_en =
    if lpe origine
    || lerasmus origine
    then
      state, None, None, None, None
    else
      match string with
      | None -> state, None, None, None, None
      | Some s when String.trim s = "" ->
        state, None, None, None, None
      | Some string ->
        let state, cursus_opt =
          Remanent_state.get_cursus
            __POS__
            ~level:string
            ?dpt:(match string, dpt with
                | "dens",_ | "autre",_
                | _,Public_data.DRI
                | _,Public_data.ENS -> None
                | _,(Public_data.ARTS
                    | Public_data.ECO | Public_data.DI | Public_data.DMA | Public_data.IBENS | Public_data.PHYS | Public_data.LILA) ->
                  Some dpt)
            ~gpscodelist
            ~year
            state
        in
        match cursus_opt with
        | None ->
          let msg =
            Format.sprintf
              "Undocumented cursus %s %s %s for %s"
              string
              (Public_data.string_of_dpt dpt)
              year
              who
          in
          Remanent_state.warn
            __POS__
            msg
            Exit
            state,
          None, None, None, None
        | Some cursus ->
          state, cursus.Public_data.entete, cursus.Public_data.entete_en,
          cursus.Public_data.pied, cursus.Public_data.pied_en
  in
  let state, key, b =
    alloc_suffix (string,dpt) state
  in
  let state, decision_opt, can_put_mean_mention =
    match dpt, string with
    | _, (None |  Some "dens") -> state, None, false
    | _, Some program ->
      match
        Remanent_state.get_decision
          ~firstname
          ~lastname
          ~year
          ~dpt
          ~program
          state
      with
      | state, Some a -> state, Some a, true
      | state, None ->
        begin
          let state, l =
            Remanent_state.get_decision_list
              ~firstname
              ~lastname
              ~dpt
              ~program
              state
          in
          let l =
            List.filter
                (fun x ->
                    x.Public_data.decision_annee = year
                 || x.Public_data.decision_validated = Some true)
                l
          in
          state, None, (List.for_all (fun x -> x.Public_data.decision_mean = None && not (x.Public_data.decision_validated = Some false)) l)
        end
  in
  let
    moyenne_opt, mention_opt, mention_en_opt,
    rank_opt, effectif_opt,
    date_opt, date_en_opt, commission_name_opt, commission_name_en_opt,
    decision_opt, decision_en_opt, validated_opt
    =
    match decision_opt with
    | None ->
      None, None, None, None, None, None, None, None, None, None, None, None
    | Some d ->
      d.Public_data.decision_mean,
      d.Public_data.decision_mention,
      d.Public_data.decision_mention_en,
      d.Public_data.decision_rank,
      d.Public_data.decision_effectif,
      d.Public_data.decision_date,
      d.Public_data.decision_date_en,
      d.Public_data.decision_commission_name,
      d.Public_data.decision_commission_name_en,
      d.Public_data.decision_decision,
      d.Public_data.decision_decision_en,
      d.Public_data.decision_validated
  in
  let state, mean =
      let state, exception_cursus =
        let rec aux state l =
          match l with
          | [] -> state, false
          | (_,_,_,_,h)::t ->
            begin
              match
                h.code_cours
              with
              | None -> aux state t
              | Some code_gps ->
                match
                  Remanent_state.get_cursus_exception
                    ~firstname ~lastname ~year ~code_gps state
                with
                | state, Some _  -> state, true
                | state, None ->
                  aux state t
            end
        in
        aux state list
      in
      let dens = string = Some "dens" in
      let natt = string = Some "" in
      let decision =
        match
          decision_opt
        with
        | None -> false
        | Some _ -> true
      in
      add_mean_diplome
        is_m2
        state
        ~dens
        ~natt
        ~exception_cursus
        ~decision
        (string,Public_data.string_of_dpt dpt)
        moyenne_opt
        mention_opt
        validated_opt
        (try int_of_string year with _ -> 0)
        mean

  in
  let () =
    if b
    then
      let () =
        Remanent_state.fprintf
          state
          "\\newcounter{validatedwogradeects%s}%%\n\ \\newcounter{grade%s}%%\n\ \\newcounter{gradedects%s}%%\n\ \\newcounter{potentialects%s}%%\n"
          key key key key
      in
      let () =
        Remanent_state.fprintf
          state
          "\\setcounter{validatedwogradeects%s}{0}%%\n\ \\setcounter{grade%s}{0}%%\n\ \\setcounter{gradedects%s}{0}%%\n\ \\setcounter{potentialects%s}{0}%%\n" key key key key
      in
      ()
  in
  let state, color =
    match
      Tools.map_opt String.trim string
    with
    | None ->
      let state =
        List.fold_left
          (fun state elt ->
             let _,_,_,_,cours = elt in
             match cours.note with
             | Some Public_data.En_cours
             | Some Public_data.Absent
             | Some Public_data.Abandon
             | Some Public_data.Temporary _
             | None ->
               state
             | Some Public_data.String _
             | Some Public_data.Float _
             | Some Public_data.Valide_sans_note ->
             Remanent_state.add_missing_ects_attribution
               state
               {
                 Public_data.missing_grade_promotion =
                   promo;
                 Public_data.missing_grade_dpt=
                   fetch_code elt ;
                 Public_data.missing_grade_dpt_indice =
                   string_of_int (fetch elt);
                 Public_data.missing_grade_teacher =
                   Tools.unsome_string
                    cours.responsable ;
                 Public_data.missing_grade_intitule =
                   Tools.unsome_string
                     cours.cours_libelle  ;
                 Public_data.missing_grade_code_gps =
                   Tools.unsome_string
                     cours.code_cours ;
                 Public_data.missing_grade_year = year ;
                 Public_data.missing_grade_lastname =
                   lastname;
                 Public_data.missing_grade_firstname =
                   firstname;
               }
          )
          state
          list
      in
      state,None
    | Some ("DENS" | "dens") -> state, Some Color.blue
    | Some ("LInfo" | "linfo" | "agreginfosu" | "agreginfoupc") ->
      state, Some Color.yellow
    | Some ("lmath" | "mmath" | "LMath" | "MMath" | "mape" | "mathfond" | "mathfondpantheonsor" | "mathfondsu" | "modsimorsay" | "modsimversailles" | "prob" | "mfimfa" |   "mfimfaorsay" |  "mmod" | "mprobfinmformens" |  "malea" | "marianageo" | "mmathgeneric" | "agregmathsu" | "agregmathupc") ->
      state, Some Color.orange
    | Some ("imalis") ->
      state, Some Color.green
    | Some ("leco" | "LEco") ->
      state, Some Color.pink
    | Some ("mphys") -> state, Some Color.duckblue
    | Some ("m" | "l" | "m1" | "l3" | "M" | "L" | "M1" | "L3" | "mva" | "mpri" | "iasd" | "mash" | "interaction" | "lmfi" | "PHILOSorbonne" | "sesi" | "alea") ->
      color_of_dpt
        who __POS__ state
        (Public_data.string_of_dpt dpt)
        origine
    | Some _  -> state, None
  in
  let state =
    List.fold_left
      (fun state elt ->
         let _,_,_,_,cours = elt in
         match cours.note with
         | None -> state
         | Some note ->
           match
             Notes.valide note,
             Notes.temporary note, cours.contrat, cours.accord
           with
           | (Some false | None), (Some false | None), _, _
           | _, _, Some true, _ | _, _, _, Some true -> state
           | _, _, (Some false | None), (Some false | None) ->
             Remanent_state.add_non_accepted_grade
               state
               {
                 Public_data.missing_grade_promotion = promo;
                 Public_data.missing_grade_dpt=
                   fetch_code elt ;
                 Public_data.missing_grade_dpt_indice =
                   string_of_int (fetch elt);
                 Public_data.missing_grade_teacher =
                   Tools.unsome_string cours.responsable ;
                 Public_data.missing_grade_intitule =
                   Tools.unsome_string cours.cours_libelle  ;
                 Public_data.missing_grade_code_gps =
                   Tools.unsome_string cours.code_cours ;
                 Public_data.missing_grade_year = year ;
                 Public_data.missing_grade_lastname = lastname;
                 Public_data.missing_grade_firstname = firstname;
               }
      )
      state list
  in
  let bgcolor=[None;color;None;None;None;None;None] in
  let () =
    Remanent_state.fprintf state
      "\\setcounter{totalrows}{%i}%%%%\n\ "
      (List.length list)
  in
  let state =
      StringOptMap.fold
        (fun (s,d) _ state ->
      Remanent_state.warn __POS__ (Format.sprintf "%s %s" (match s with None -> "none" | Some x -> x) d) Exit state )
      cursus_map state
    in
  let dpt' =
    match dpt, string with
    | _, (None |  Some "dens") -> "dens"
    |_, _ -> Public_data.string_of_dpt dpt
  in
  let state, foot_english =
    (match
      StringOptMap.find_opt
        (string,dpt')
        cursus_map, footpage_en
      with
    | None, _
    | Some (_,None),_
    | _,None -> state, ""
    | Some (_,Some x),Some y ->
    let state, b = print_foot_note string dpt x year y state in
    if b then
       state, Format.sprintf
         "\\footnote{%s}"
         y
     else
       state,"")
  in
  let state, foot_french =
    (match
      StringOptMap.find_opt
        (string,dpt')
        cursus_map, footpage
      with
    | None, _ -> Remanent_state.warn __POS__ (Format.sprintf "FRENCHNONE %s %s"
   (match string with None -> "none" | Some x -> x) (Public_data.string_of_dpt dpt)) Exit state, ""
    | Some (_,None),_ -> Remanent_state.warn __POS__ (Format.sprintf "FRENCHNOENDDATE %s %s"
   (match string with None -> "none" | Some x -> x) (Public_data.string_of_dpt dpt)) Exit state, ""
    | _,None ->
     Remanent_state.warn __POS__ (Format.sprintf "FRENCH %s %s"
    (match string with None -> "none" | Some x -> x) (Public_data.string_of_dpt dpt)) Exit state, ""
    | Some (_,Some x),Some y ->
    let state, b = print_foot_note string dpt x year y state in
    if b then
       state, Format.sprintf
         "\\footnote{%s}"
         y
     else
       state,"")
  in

  let () =
    match entete,entete_en with
    | None, None -> ()
    | Some x, None | None, Some x ->
      let state =
        Remanent_state.warn
          __POS__
          (Format.sprintf "Missing translation for %s" x)
          Exit
          state
      in
      let state,s = Remanent_state.bilingual_string
      ~english:(Format.sprintf "%s%s" x foot_english)
      ~french:(Format.sprintf "%s%s" x foot_french) state in
      Remanent_state.fprintf state "%s" s
    | Some x, Some y ->
    let state,s = Remanent_state.bilingual_string
    ~english:(Format.sprintf "%s%s" y foot_english)
    ~french:(Format.sprintf "%s%s" x foot_french) state in
    Remanent_state.fprintf state "%s" s
  in
  let _ =
    Remanent_state.print_newline state
  in
  let state =
    Remanent_state.open_array
      __POS__
      ~bgcolor
      ~size
      ~with_lines:true
      ~title:[["Code"];["Dipl\\^ome"];["Intitul\\'e"];
              ["Enseignant"];["Semestre"];["Note"];["ECTS"]]
      ~title_english:[["Code"];["Diploma"];["Course"];
              ["Teacher"];["Semester"];["Grade"];["ECTS"]]
      state
  in
  let macro = "cours" in
  let state, monsieur =
    Remanent_state.bilingual_string
      ~french:"M."
      ~english:"Mr"
      state
  in
  let state, madame =
    Remanent_state.bilingual_string
      ~french:"Mme"
      ~english:"Mrs"
      state
  in
  let list = Tools.sort fetch p list in
  let state, mean, dens, natt, cours_list, stage_list  =
    List.fold_left
      (fun
        (state, (mean:(bool * float * (Public_data.note option * float option) list * int)
         StringOptMap.t * (StringOptMap.key * float option * string option * bool option * int)
         list), dens, natt, cours_list, stage_list)
        (is_m2,dpt_en,(diplome:string),diplome_en,cours) ->
        let () =
          Remanent_state.open_row ~macro state
        in
        let codecours =
          string_of_stringopt cours.code_cours
        in
        let state, compensation =
          Remanent_state.get_compensation
            state
            ~firstname ~lastname ~year ~codecours
        in
        let () =
          match
            compensation
          with
          | Some _ ->
            Remanent_state.print_optional_cell
              "compensation"
              state
          | None -> ()
        in
        let state =
          if
            match cours.note with
            | None -> true
            | Some a ->
              Notes.en_cours a
          then
            let dpt = fetch_code (is_m2,dpt_en,diplome,diplome_en,cours) in
            let dpt_indice =
              string_of_int (fetch (is_m2,dpt_en,diplome,diplome_en,cours))
            in
            Remanent_state.add_missing_grade
              state
              {
                Public_data.missing_grade_promotion =
                  promo;
                Public_data.missing_grade_firstname =
                  firstname ;
                Public_data.missing_grade_lastname =
                  lastname  ;
                Public_data.missing_grade_year = year ;
                Public_data.missing_grade_dpt = dpt ;
                Public_data.missing_grade_dpt_indice = dpt_indice ;
                Public_data.missing_grade_code_gps =                              Tools.unsome_string cours.code_cours;
                Public_data.missing_grade_teacher = Tools.unsome_string cours.responsable;
                Public_data.missing_grade_intitule = Tools.unsome_string cours.cours_libelle
              }
          else
            state
        in
        let () =
          Remanent_state.print_cell
            codecours
            state
        in
        let state, diplome =
          Remanent_state.bilingual_string
            ~english:diplome_en
            ~french:diplome
            state
        in
        let () =
          Remanent_state.print_cell
            diplome
            state
        in
        let state, f =
          special_course state cours
        in
        let state, libelle, course_name_translation, course_entry =
          match cours.cours_libelle with
          | None -> state,
                    None,
                    Public_data.empty_course_name_translation, Public_data.empty_course_entry
          | Some l ->
            let course_name_translation =
              {Public_data.empty_course_name_translation
               with
                Public_data.code=codecours;
                Public_data.name=Some l}
            in
            let course_entry =
              {
                Public_data.empty_course_entry
                with
                  Public_data.gps_entry = l
              }
            in
            if is_stage cours
            then
              begin
                let internship =
                  {
                    Public_data.missing_internship_promotion = promo ;
                    Public_data.missing_internship_year=year;
                    Public_data.missing_internship_firstname=firstname;
                    Public_data.missing_internship_lastname=lastname;
                    Public_data.missing_internship_intitule=
                      Tools.unsome_string  cours.cours_libelle ;
                    Public_data.missing_internship_code_gps=
                      Tools.unsome_string
                        cours.code_cours
                  }
                in
                let state, stage_opt =
                  fetch_stage
                    state
                    ~internship
                    cours.commentaire stages
                in
                match stage_opt with
                | None ->
                  begin
                    let state, l, l_en =
                      match
                        Remanent_state.get_course_name_translation
                          ~label:l
                          ~codegps:codecours
                          ~year
                          state
                      with
                      | state, (None,None) ->
                        let state =
                          Remanent_state.add_missing_course_name_translation
                            state
                            course_name_translation
                        in
                        let state =
                          Remanent_state.add_missing_course_entry
                            state
                            {Public_data.empty_course_entry with
                             Public_data.gps_entry = l}
                        in
                        state, Some l, None
                      | state, (lib, lib_en) ->
                        let lib =
                          match lib with
                          | None -> Some l
                          | a -> a
                        in
                        let state, lib_en =
                          let course_entry =
                            {course_entry
                             with Public_data.french_entry=lib;
                                  Public_data.english_entry=lib_en}
                          in
                          let course_name_translation =
                            {course_name_translation
                             with
                              Public_data.name=lib;
                              Public_data.name_en=lib_en}
                          in
                          match lib, lib_en with
                          | None, None -> state, None
                          | Some _, Some y ->
                            let y = String.trim y in
                            if y = ""
                            ||
                            (String.sub y 0 1 = "\"" &&
                             String.trim (String.sub y 1 ((String.length y)-1))
                             = "\"")
                            then
                              let state =
                                Remanent_state.add_missing_course_entry
                                  state
                                  course_entry
                              in
                              Remanent_state.add_missing_course_name_translation
                                state
                                course_name_translation, lib
                            else
                              let state =
                                Remanent_state.add_course_entry_in_report
                                  Collect_course_entries.unify_course_entry
                                  __POS__
                                  course_entry
                                  state
                              in
                              state, lib_en
                          | None, Some x | Some x, None ->
                            let state =
                              Remanent_state.add_missing_course_name_translation
                                state
                                course_name_translation
                            in
                            let state =
                              Remanent_state.add_missing_course_entry
                                state
                                course_entry
                            in
                            state, Some x
                        in
                        state, lib, lib_en
                    in
                    let state, libelle =
                      Remanent_state.bilingual_string
                        ?english:l_en
                        ~french:(string_of_stringopt l)
                        state
                    in
                    state, Some libelle, course_name_translation, course_entry
                  end
                | Some stage ->
                  let issue =
                    match
                      cours.note
                    with
                    | Some Public_data.En_cours
                    | Some Public_data.Absent
                    | Some Public_data.Abandon
                    | Some Public_data.Temporary _
                    | None ->
                      begin
                        match stage.stage_valide with
                        | None
                        | Some (Public_data.Abs | Public_data.Bool false) ->
                        false
                        | Some (Public_data.Bool true) -> true
                      end
                    | Some Public_data.String _
                    | Some Public_data.Float _
                    | Some Public_data.Valide_sans_note ->
                      begin
                        match stage.stage_valide, stage.stage_accord with
                        | (None | Some (Public_data.Abs | Public_data.Bool
                                        false)), _
                        | _, (None | Some false) ->
                          true
                        | Some (Public_data.Bool true), Some true -> false
                      end
                  in
                  let state =
                    if issue then
                      Remanent_state.add_non_validated_internship
                        state internship
                    else
                      state
                  in
                  let sujet =
                    match stage.sujet with
                    | None -> ""
                    | Some a ->
                      if l = ""
                      then a
                      else "\\newline \""^a^"\""
                  in
                  let state, directeur =
                    match stage.directeur_de_stage with
                    | None -> state, ""
                    | Some a ->
                      if (Special_char.lowercase
                            (String.trim a) = "non applicable") then state, ""
                      else
                      if (l = "" && sujet="")
                      then state, a else
                        let state, directed =
                          Remanent_state.bilingual_string
                            ~english:"under the supervision of"
                            ~french:"dirigé par"
                            state
                        in
                        state, Format.sprintf
                          "\\newline %s %s" directed a
                  in
                  let state, l, l_en =
                    match
                      Remanent_state.get_course_name_translation
                        ~label:l
                        ~codegps:codecours
                        ~year
                        state
                    with
                    | state, (None,None) ->
                      let state =
                        Remanent_state.add_missing_course_name_translation
                          state
                          course_name_translation
                      in
                      let state =
                        Remanent_state.add_missing_course_entry
                          state
                          course_entry
                      in
                      state, Some l, None
                    | state, (lib, lib_en) ->
                      let lib =
                        match lib with
                        | None -> Some l
                        | a -> a
                      in
                      let course_entry =
                        {course_entry
                         with Public_data.french_entry=lib;
                              Public_data.english_entry=lib_en}
                      in
                      let course_name_translation =
                        {course_name_translation
                         with
                          Public_data.name=lib;
                          Public_data.name_en=lib_en}
                      in
                      let state, lib_en =
                        match lib, lib_en with
                        | None, None -> state, None
                        | Some _, Some y ->
                          if String.trim y = ""
                          then
                            let state =
                              Remanent_state.add_missing_course_entry
                                state
                                course_entry
                            in
                            Remanent_state.add_missing_course_name_translation
                              state
                              course_name_translation, lib
                          else
                            let state =
                              Remanent_state.add_course_entry_in_report
                                Collect_course_entries.unify_course_entry
                                __POS__
                                course_entry
                                state
                            in
                            state, lib_en
                        | None, Some x | Some x, None ->
                          let state =
                            Remanent_state.add_missing_course_name_translation
                              state
                              course_name_translation
                          in
                          let state =
                            Remanent_state.add_missing_course_entry
                              state
                              course_entry
                          in
                          state, Some x
                      in
                      state, lib, lib_en
                  in
                  let state, libelle =
                    Remanent_state.bilingual_string
                      ?english:l_en
                      ~french:(string_of_stringopt l)
                      state
                  in
                  state,
                  Some
                    (Format.sprintf "%s%s%s" libelle sujet directeur),
                  course_name_translation, course_entry
              end
            else state, Some l,course_name_translation, course_entry
        in
        let state, libelle, libelle_en =
          if is_stage cours then state, libelle, None
          else
          if String.trim codecours = ""
          then
            if libelle = Some "N/A" then state, libelle, libelle
            else if libelle = Some "Points de jury" then state, libelle, Some "Jury credits"
            else
              let state =
                Remanent_state.warn
                  __POS__
                  (Format.sprintf
                     "Incoherent empty CODE GPS with course name %s "
                     (Tools.unsome_string libelle))
                  Exit
                  state
              in
              state, libelle, None
          else
            match
              Remanent_state.get_course_name_translation
                ~label:(match libelle with Some a -> a | None -> "")
                ~codegps:codecours
                ~year
                state
            with
            | state, (None,None) ->
              let state =
                Remanent_state.add_missing_course_name_translation
                  state
                  course_name_translation
              in
              let state =
                Remanent_state.add_missing_course_entry
                  state
                  course_entry
              in
              state, libelle, None
            | state, (lib, lib_en)  ->
              let course_entry =
                {course_entry
                 with Public_data.french_entry=lib;
                      Public_data.english_entry=lib_en}
              in
              let course_name_translation =
                {course_name_translation
                 with
                  Public_data.name=lib;
                  Public_data.name_en=lib_en}
              in
              let state =
                match lib, lib_en with
                | None, None -> state
                | Some _, Some _ ->
                  Remanent_state.add_course_entry_in_report
                    Collect_course_entries.unify_course_entry
                    __POS__
                    course_entry
                    state
                | None, Some _ | Some _, None ->
                  let state =
                    Remanent_state.add_missing_course_entry
                      state
                      course_entry
                  in
                  Remanent_state.add_missing_course_name_translation
                    state
                    course_name_translation
              in
              state, lib, lib_en
        in
        let state, libelle =
          Remanent_state.bilingual_string
            ?english:libelle_en
            ~french:(string_of_stringopt libelle)
            state
        in
        let () =
          Remanent_state.print_cell
            (f libelle)
           state
        in
        let state, responsable_opt =
          match cours.code_cours with
          | Some codegps ->
            Remanent_state.get_course_exception
              ~codegps
              ~year
              state
          | None -> state, None
        in
        let state, (genre, firstname, lastname) =
            match responsable_opt with
          | None ->
            if match cours.responsable with None -> true | Some x when String.trim x = "" -> true | Some _ -> false
            then state, (Public_data.Unknown, "", "")
            else
                let a,b,c =
                    Special_char.split_name
                        (string_of_stringopt cours.responsable)
                in
                let state, a  =
                  match Special_char.lowercase a with
                  | "m" | "mr" | "monsieur" | "m." | "mr." -> state, Public_data.Masculin
                  |  "mlle" | "mme" | "mlle." | "mme." | "madame" | "mademoiselle" -> state, Public_data.Feminin
                  | x -> Remanent_state.warn __POS__ (Format.sprintf "Unknown gender (%s) %s %s" x (match cours.responsable with None -> "none" | Some s -> s) libelle)  Exit state, Public_data.Unknown
                in state, (a,b,c)

          | Some a ->
                     state, (a.Public_data.course_exception_genre,
                      a.Public_data.course_exception_firstname,
                      a.Public_data.course_exception_lastname)
        in
        let responsable =
            Format.sprintf "%s %s %s"
              (match
                 genre
               with
               | Public_data.Masculin -> monsieur
               | Public_data.Feminin -> madame
               | Public_data.Unknown -> "")
              (Special_char.capitalize firstname)
              (Special_char.uppercase lastname)
        in
        let () =
          Remanent_state.print_cell
            responsable
            state
        in
        let () =
          Remanent_state.print_cell
            (string_of_stringopt cours.semestre)
            state
        in
        let state, note_string =
          match cours.note with
          | None -> state, ""
          | Some f -> Notes.to_string __POS__ state f
        in
        let () =
          Remanent_state.print_cell
            note_string
            state
        in
        let () =
          Remanent_state.print_cell
            (Notes.string_of_ects cours.ects)
            state
        in
        let () =
          Remanent_state.close_row state
        in
        let () =
          Remanent_state.fprintf state "%%\n\ "
        in
        let state, mean, dens, natt, cours_list, stage_list =
          if year > current_year
          (*|| not ((do_report report || keep_success || keep_faillure)*)
          then state, mean, dens, natt, cours_list, stage_list
          else
            match Tools.map_opt String.trim string
            with
            | None
            | Some ""->
              let state, cours_list, natt =
                add_dens state year compensation cours cours_list natt
              in
                state, mean, dens, natt, cours_list, stage_list

            | Some ("dens" | "DENS") ->
              let state, cours_list, dens =
                add_dens state year compensation cours cours_list dens
              in
              state, mean, dens, natt, cours_list, stage_list
            | Some _ ->
              let state, mean, cours_list, dens =
                add_mean is_m2 state
                  (string,(Public_data.string_of_dpt dpt)) compensation cours
                cours_list year
                mean dens
              in state, mean, dens, natt, cours_list, stage_list
        in
        state,mean, dens, natt, cours_list, stage_list)
      (state,mean,dens,natt, cours_list, stage_list)
      list
  in
  let () =
    Remanent_state.close_array state
  in
  let () =
    Remanent_state.fprintf
      state
      "\n\ \n\ \\vspace*{2mm}\n\ \n\ "
  in
  let () =
    Remanent_state.log
      state
      "\\addtocounter{validatedwogradeects%s}{\\thevsnects}%%\n\ \\addtocounter{grade%s}{\\thetotal}%%\n\ \\addtocounter{gradedects%s}{\\theects}%%\n\ \\addtocounter{potentialects%s}{\\thepotentialects}" key key key key
  in
  let moyenne_value =
    Format.sprintf
      "\\thegrade%s/\\thegradedects%s"
      key
      key
  in
  let state, moyenne =
    Remanent_state.bilingual_string ~french:"Moyenne :" ~english:"Mean:" state
  in
  let state, moyenne_provisoire =
    Remanent_state.bilingual_string ~french:"Moyenne provisoire :" ~english:"Temporary mean:" state
  in
  let state, bien =
    Remanent_state.bilingual_string
      ~english:"Distinction: \textbf{Upper Second-Class Honours}"
      ~french:"Mention : \\textbf{Bien}"
      state
  in
  let state, assez_bien =
    Remanent_state.bilingual_string
      ~french:"Mention : \\textbf{Assez Bien}"
      ~english:"Distinction: \\textbf{Lower Second-Class Honours}"
      state
  in
  let state, tres_bien =
    Remanent_state.bilingual_string
      ~english:"Distinction: \\textbf{First Class Honour}"
      ~french:"Mention : \\textbf{Très Bien}"
      state
  in
  let no_definitive_ects,
      not_enough_ects,
      moyenne, update_moyenne, mention =
    if (not can_put_mean_mention)
    || string = Some "DENS"
    || string = Some "dens"
    || string = None
    || string = Some ""
    then "","","","",""
    else
      let mean =
        Format.sprintf
          "\\numprint{\\fpeval{%s}}"
          moyenne_value
      in
      let no_definitive_ects =
        Format.sprintf
          "\\thegradedects%s=0"
          key
      in
      let not_enough_ects =
        Format.sprintf
          "\\fpeval{(\\thegradedects%s+\\thevalidatedwogradeects%s+\\thepotentialects%s)} < \\fpeval{60*\\factorsquare}"
          key key key
      in
      let definitive =
        Format.sprintf
          "\\thepotentialects%s=0"
          key
      in
      let mean_string =
        Latex_helper.case
          Latex_helper.ifnum
          [ no_definitive_ects,"";
            not_enough_ects,"";
            definitive,
            Format.sprintf "%s \\textbf{\\numprint{\\fpeval{\\mean}}}/20 \\hspace*{1cm}%%\n\ " moyenne]
          ~otherwise:(Format.sprintf "%s \\numprint{\\fpeval{\\mean}}/20 \\hspace*{1cm}%%\n\ " moyenne_provisoire)
      in
      let update_mean =
        match moyenne_opt with
        | None ->
          Format.sprintf
            "\\renewcommand{\\mean}{%s}"
            moyenne_value
        | Some m ->
          Format.sprintf
            "%%Moyenne changee en commission\n\ %s%%Nouvelle moyenne\n\ %s\n\ %%\\renewcommand{\\mean}{%s}\n\ \\renewcommand{\\mean}{%f}"
            (Latex_helper.comment mean)
            (Latex_helper.comment
               (Format.sprintf "%f" m))
            moyenne_value
            m
      in
      let mention =
        if
          (not can_put_mean_mention)
          || string = Some "DENS"
          || string = Some "dens"
          || string = None
          || string = Some ""
        then ""
        else
          let mention =
            Latex_helper.case
              Latex_helper.ifnum
              [no_definitive_ects,"";
               not_enough_ects,"";
               definitive,
               Latex_helper.case
                 Latex_helper.ifnum
                 [
                   Format.sprintf
                     "\\fpeval{\\mean<12}  = 1","";
                   Format.sprintf
                     "\\fpeval{\\mean<14} = 1",
                   assez_bien;
                   Format.sprintf
                     "\\fpeval{\\mean<16} = 1 ",
                   bien;
                 ]
                 ~otherwise:tres_bien]
              ~otherwise:""
          in
          match mention_opt, mention_en_opt with
          | None,None -> mention
          | Some a, Some b ->
            snd
              (Remanent_state.bilingual_string
                ~french:(Format.sprintf "Mention : \\textbf{%s} \\hspace*{1cm}" a)
              ~english:(Format.sprintf "Distinction: \\textbf{%s} \\hspace*{1cm}" b)
              state)
          | Some a, None ->
            let a' = simplify_string a in
            if a' = "" then ""
            else
            if a' = "tres bien" || a' = "tb" then tres_bien
            else if a' = "bien" || a' = "b" then bien
            else if a' = "assez bien" || a' = "ab" then assez_bien
            else
              Format.sprintf "Mention : \\textbf{%s} \\hspace*{1cm}" a
          | None, Some a ->
            let a' = simplify_string a in
            if a' = "" then ""
            else
            if a' = "first class honour" then tres_bien
            else if a' = "upper second-class honours" then bien
            else if a' = "lower second-class honours"  then assez_bien
            else
              Format.sprintf "Disctinction: \\textbf{%s} \\hspace*{1cm}" a
      in
      no_definitive_ects, not_enough_ects,
      mean_string, update_mean, mention
  in
  let state, potentiellement =
    Remanent_state.bilingual_string
      ~english:"potentially"
      ~french:"potentiellement"
      state
  in
  let state,ects,pects =
    let
      this_year_ects_amp =
      Format.sprintf
        "\\fpeval{(\\theects+\\thevsnects)}"
    in
    let total_ects_amp =
      Format.sprintf
        "\\fpeval{(\\thegradedects%s+\\thevalidatedwogradeects%s)}"
        key key
    in
    let total_ects =
      Format.sprintf
        "\\fpeval{(\\thegradedects%s+\\thevalidatedwogradeects%s)/\\factorsquare}"
        key key
    in
    let state, ects =
      Remanent_state.bilingual_string
        ~french:"ECTS "
        ~english:"ECTS"
        state
    in
    let state, cumulated =
      Remanent_state.bilingual_string
        ~french:"(cumulés) "
        ~english:"(cumulated)"
        state
    in
    let ects_string =
      Latex_helper.case
        Latex_helper.ifnum
        [
          Format.sprintf "%s=0" total_ects_amp,"";
          Format.sprintf "%s=%s" this_year_ects_amp
            total_ects_amp,
          Format.sprintf "%s: %s" ects total_ects;
        ]
        ~otherwise:(Format.sprintf
                      "%s %s: %s"
                      ects cumulated total_ects)
    in
    let potential_ects_string  =
      Latex_helper.ifnum
        ~cond:(Format.sprintf "\\thepotentialects%s=0" key)
        ~btrue:""
        ~bfalse:(Format.sprintf "\\hspace*{0.2cm} (%s  {{\\fpeval{(\\thegradedects%s+\\thevalidatedwogradeects%s+\\thepotentialects%s)/\\factorsquare}}} ects)" potentiellement key key key)
        ()
    in
    state, ects_string, potential_ects_string
  in
  let rank =
    match rank_opt, effectif_opt with
    | None, _ -> ""
    | Some a, None ->
      Format.sprintf "Rang : %i \\hspace*{1cm}" a
    | Some a, Some b ->
      Format.sprintf "Rang : %i/%i \\hspace*{1cm}" a b
  in
  let undefine a =
      let a = String.trim a in
      match String.index_opt a ' ' with
        | Some i ->
          let article = String.sub a 0 i in
          let suite = String.sub a (i+1) (String.length a - (i+1)) in
          begin
            match Special_char.lowercase article with
            | "le" -> Format.sprintf "du %s" suite
            | _ -> Format.sprintf "de %s" a
          end
      | None -> Format.sprintf "de %s" a
  in
  let set_date b =
  match String.rindex_opt b ' ' with
    | Some i ->
      let article = String.sub b 0 i in
      begin
          match Special_char.lowercase article with
          | "session" -> Format.sprintf " (%s)" b
          | _ -> Format.sprintf "du %s" b
      end
  | None -> Format.sprintf "du %s" b
  in
  let set_date_en b =
  match String.rindex_opt b ' ' with
    | Some i ->
      let article = String.sub b 0 i in
      begin
        match Special_char.lowercase article with
        | "session" -> Format.sprintf " (%s)" b
        | _ -> Format.sprintf "of %s" b
      end
  | None -> Format.sprintf "of %s" b
  in
  let commission =
    match
      commission_name_opt, date_opt
    with
    | None, _ -> ""
    | Some a, None ->
      let a = undefine a in
      Format.sprintf
        "Décision %s \n\n"
        a
    | Some a, Some b ->
      let a = undefine a in
      let b = set_date b in
      Format.sprintf
        "Décision %s %s \n\n"
        a b
  in
  let commission_en =
    match
      commission_name_en_opt, date_en_opt
    with
    | None, _ -> None
    | Some a, None ->
      Some (Format.sprintf
        "Decision of the %s \n\n"
        a)
    | Some a, Some b ->
      let b = set_date_en b in
      Some (Format.sprintf
        "Decision of the %s %s \n\n"
        a b)
  in
  let state, commission =
    match commission_en with
    | None -> state, commission
    | Some english ->
      Remanent_state.bilingual_string ~english ~french:commission state
  in
  let state, decision =
    match decision_opt, decision_en_opt with
    | None, None -> state, ""
    | Some x, None | None, Some x  ->
      state, Format.sprintf "%s \\hspace*{1cm}" x
    | Some x, Some y ->
      Remanent_state.bilingual_string ~english:y ~french:x state
  in
  let lineproportion = 0.9 in
  let () =
    Remanent_state.log
      ~lineproportion
      state
      "%s"
      commission
  in
  let () =
    Remanent_state.print_newline state
  in
  let () =
    Remanent_state.fprintf
      state
      "\\nprounddigits{2}%%\n\ "
  in
  let () =
    Remanent_state.fprintf
      state
      "%s"
      update_moyenne
  in
  let lineproportion = 0.45 in
  let () =
    if (not (string = Some "DENS"
             || string = Some "dens"
             || string = None
             || string = Some "")) && can_put_mean_mention
    then
      let s =
        Remanent_state.log_to_string
          ~lineproportion
          state
          moyenne
      in
      let s =
        Latex_helper.case
          Latex_helper.ifnum
          [ no_definitive_ects,"";
            not_enough_ects,"";
          ]
          ~otherwise:s
      in
      let () =
        Remanent_state.fprintf
          state
          "%s"
          s
      in
      ()
  in
  let lineproportion = 0.45 in
  let () =
    (fun s ->
       if s = "" then () else
         Remanent_state.log_string
           ~lineproportion
           state
           s)
      (
        if ects = "" then pects
        else if pects = ""
        then ects
        else
          ects^pects)
  in
  let () =
    Remanent_state.fprintf
      state
      "\\npnoround%%\n\ \n\n"
  in
  let () = Remanent_state.print_newline state in
  let () =
    List.iter
      (fun (s,lineproportion) ->
         if s = "" then () else
           Remanent_state.log_string
             ~lineproportion
             state
             s)
      [decision,0.45;rank,0.25;mention,0.25]
  in
  let () = Remanent_state.print_newline state in
  let () = Remanent_state.fprintf state "\\vfill\n\ " in
  let () = Remanent_state.print_newline state in
  let () = Remanent_state.print_newline state in
  let () = Remanent_state.print_newline state in
  state,mean,dens,natt, cours_list, stage_list

let good (a,_) =
  match a with
  | None -> false
  | Some a ->
    List.mem a ["l";"m"]

let build_gpscodelist ~year ~firstname ~lastname  situation state =
    let gpscodelist =
      List.fold_left
       (fun acc diplome ->
          match diplome.diplome_diplome
          with
          | None -> acc
          | Some a -> a::acc)
       [] situation.diplomes
    in
    let gpscodelist = fill_gpscodelist ~year ~firstname ~lastname gpscodelist situation state in
    state, {situation with gpscodelist}

let export_transcript
    ~output
    ?language
    ?bilinguage
    ?include_picture
    ?repartition
    ?signature
    ?report
    ?filter:(remove_non_valided_classes=Public_data.All_but_in_progress_in_current_academic_year)
    ?keep_success
    ?keep_faillure
    state gps_file =
  let signature =
    match signature with
    | None -> []
    | Some l -> l
  in
  let keep_success =
    match keep_success with
    | None -> false
    | Some b -> b
  in
  let keep_faillure =
    match keep_faillure with
    | None -> false
    | Some b -> b
  in
  let state, language =
    Tools.get_option
      state
      Remanent_state.get_language
      language
  in
  let state, bilinguage =
    Tools.get_option
      state
      Remanent_state.get_is_bilingual
      bilinguage
  in
  let state, repartition =
    Tools.get_option
      state
      Remanent_state.get_repartition
      repartition
  in
  let state, include_picture =
    Tools.get_option
      state
      Remanent_state.get_include_pictures
      include_picture
  in
  let alloc_suffix =
    let l0 =
      ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j";"k";"l";"m";"n"]
    in
    let m = ref StringOptMap.empty in
    let l = ref l0 in
    let rec next state =
      match !l with
      | h::t ->
        let () = l:=t in
        state, h
      | [] ->
        let state =
          Remanent_state.warn
            __POS__
            "Too many cursus"
            Exit
            state
        in
        let () = l:=l0 in
        next state
    in
    let alloc id state =
      match StringOptMap.find_opt id (!m) with
      | None ->
        let state, key = next state in
        let () = m := StringOptMap.add id key (!m) in
        state, key, true
      | Some key ->
        state, key, false
    in
    alloc
  in
  let alloc_suffix a b =
    alloc_suffix (fst a,Public_data.string_of_dpt (snd a)) b
  in
  let state, rep =
    Safe_sys.rec_mk_when_necessary __POS__
      state (fst output)
  in
  let file = snd output in
  let file =
    if rep = ""
    then
      file
    else
      Printf.sprintf "%s/%s" rep file
  in
  let state, output_channel_opt =
    try
      state, Some (open_out file)
    with exn ->
      let msg = Printexc.to_string exn in
      let () =
        Format.printf
          "Cannot open file %s (%s)@."
          file
          msg
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s (%s)"  file msg)
        Exit
        state ,
      None
  in
  match output_channel_opt with
  | None -> state, None
  | Some out ->
    let mode = Loggers.Latex
        {Loggers.orientation = Loggers.Landscape ;
         Loggers.language =
           (match language with
           | Public_data.French -> Loggers.French
           | Public_data.English -> Loggers.English );
         Loggers.bilinguage =
           bilinguage
        }
    in
    let logger = Loggers.open_logger_from_channel ~mode out in
    let old_logger = Remanent_state.save_std_logger state in
    let state = Remanent_state.set_std_logger state logger in
    let lastname =
      Special_char.uppercase (Tools.unsome_string gps_file.nom)
    in
    let firstname =
      Special_char.capitalize (Tools.unsome_string gps_file.prenom)
    in
    let promo =
      (Tools.unsome_string gps_file.promotion)
    in
    let state, promo_int =
      try
        state, int_of_string promo
      with
        _ ->
        Remanent_state.warn
          __POS__
          (Format.sprintf
             "Promotion should be an integer %s %s %s"
             promo
             firstname
             lastname)
          Exit
          state,
        0
    in
    let state, situation =
      Public_data.YearMap.fold
        (fun year situation (state,map) ->
            let state, situation =
              build_gpscodelist ~year ~firstname ~lastname situation state
            in
            state, Public_data.YearMap.add year situation map)
        gps_file.situation (state,gps_file.situation)
    in
    let gps_file = {gps_file with situation} in
    let state, promo_int =
      Public_data.YearMap.fold
        (fun year _ (state, y') ->
           let state, y_int =
             try
               state, int_of_string year
             with
               _ ->
               Remanent_state.warn
                 __POS__
                 (Format.sprintf
                    "Promotion should be an integer %s %s %s"
                    year
                    firstname
                    lastname)
              Exit
              state,
               y'
           in
           state, min y' y_int)
        gps_file.situation
        (state, promo_int)
    in
    let promo = string_of_int promo_int in
    let who =
      Format.sprintf
        "pour %s %s (%s)"
        firstname lastname promo
    in
    let state, additional_courses =
      Remanent_state.get_additional_course
        ~firstname ~lastname
        state
    in
    let state, gps_file =
      List.fold_left
        (fun (state, gps_file) course ->
           add_extra_course state course gps_file)
        (state, gps_file)
        additional_courses
    in
    let l = Public_data.YearMap.bindings gps_file.situation in
    let state, current_year =
      Remanent_state.get_current_academic_year state
    in
    let state, l_rev =
    List.fold_left
      (fun (state, l) (y,annee) ->
      let state, cours =
        List.fold_left
          (fun (state, l) cours ->
             let state, cours =
               match cours.code_cours with
               | None -> state, cours
               | Some code ->
                 let state,cours =
                   match
                     Remanent_state.get_note_a_modifier
                       ~firstname ~lastname
                       ~year:y
                       ~code
                       state
                   with
                   | state, None -> (state,cours)
                   | state, Some note ->
                     let state, note =
                          Notes.of_string __POS__ state note
                            (Some (Public_data.Bool true))
                    in
                     state, {cours with note}
                 in
                 match
                   Remanent_state.get_ects_a_modifier
                     ~firstname ~lastname
                     ~year:y
                     ~code
                     state
                 with
                 | state, None -> (state,cours)
                 | state, Some ects ->
                   let ects = Some ects in
                   state, {cours with ects}
                 in
             state, cours::l)
          (state,[])
          (List.rev annee.cours)
      in
      let annee = {annee with cours} in
      state, (y,annee)::l)
      (state, [])
      l
    in
    let l = List.rev l_rev in
    let state,l_rev,_ =
      List.fold_left
        (fun (state,l,counter) (y,annee) ->
           if
             (not (lechange_dri annee))
             &&
             begin
               match annee.situation_administrative
               with
               | None ->
                 begin
                   try int_of_string y<=2015
                   with _ -> false
                 end
                 ||
                 begin
                   match
                     annee.code_option
                   with
                   | Some "OPT1" -> true
                   | Some _ | None -> false
                 end
                 ||
                 counter = 0
               | Some sit ->
                 (
                   (simplify_string sit = "scolarite a l'ens"
                    &&
                    not
                      (List.exists
                         (fun dip ->
                            let code = dip.diplome_diplome in
                            match code with
                            | None -> false
                            | Some dip ->
                              if String.length dip < 3 then false
                              else String.sub dip 0 3 = "CES")
                         annee.diplomes))
                   ||
                   (simplify_string sit = "autre cas"
                    &&
                    (not
                       (List.exists
                          (fun dip ->
                             let code = dip.diplome_diplome in
                             match code with
                             | None -> false
                             | Some dip ->
                               if String.length dip < 3 then false
                               else String.sub dip 0 3 = "CES")
                          annee.diplomes))
                    &&
                    (annee.derniere_annee = Some true
                     || begin
                       match
                         annee.code_option
                       with
                       | Some "OPT1" -> true
                       | Some _ | None -> false
                     end))
                 )
             end
           then
             let counter = counter + 1 in
             let nannee = Some counter in
             let annee = {annee with nannee} in
             state,(y,annee)::l,counter
           else
             state, (y,annee)::l,counter)
        (state,[],0) l
    in
    let gps_file =
      List.fold_left
        (fun gps_file (y,annee) ->
           let situation = gps_file.situation in
           match
             Public_data.YearMap.find_opt y situation
           with
           | None -> gps_file
           | Some bilan ->
             let bilan = {bilan with nannee = annee.nannee} in
             let situation = Public_data.YearMap.add y bilan situation in
             {gps_file with situation})
        gps_file
        l_rev
    in
    let state, picture_list =
      if include_picture
      then
        Photos.get
          ~firstname ~lastname ~promo state
      else
        state, []
    in
    let state, b =
      let rec aux state l =
        match l with
        | [] -> state, false
        | h::t ->
          let state, b = Safe_sys.file_exists __POS__ state h
          in
          if b then
            state, true
          else aux state t
      in aux state picture_list
    in
    let state =
      if b || not include_picture
      then
        state
      else
        Remanent_state.add_missing_picture
          state
          {
            Public_data.student_firstname_report = firstname ;
            Public_data.student_lastname_report = lastname ;
            Public_data.student_promo_report = promo ;
          }
    in
    let state, picture_list =
      List.fold_left
        (fun (state, list) file ->
           let state, b = Safe_sys.file_exists __POS__ state file
           in
           if b then
             let state, b = Safe_sys.is_empty __POS__ state file in
             if b then state, list
             else
               state, file::list
           else
             state, file::list
        )
        (state,[])
        (List.rev picture_list)
    in
    let stages =
        gps_file.stages
    in
    let state, origine =
      get_origine who promo gps_file state
    in
    let state, cursus_map, l =
      List.fold_left
        (fun (state, cursus_map, l) (year, situation) ->
           let state, filtered_classes =
             filter_class ~firstname ~lastname ~year
               state remove_non_valided_classes
               situation.cours
           in
           let state, (cursus_map, split_cours) =
             List.fold_left
               (fun (state,
                     (cursus_map,
                      course_map)) elt ->
                 match elt.code_cours with
                 | Some code_cours  ->
                   let state,
                       (diplome_key,diplome_label,diplome_label_en,
                        diplome_dpt,diplome_dpt_en,dispense,is_m2)
                     =
                     translate_diplome
                       ~origine ~situation ~firstname
                       ~lastname
                       ~year ~code_cours state
                       elt.diplome
                   in
                   let state, cursus_map =
                     addfirstlast
                       state
                       (diplome_key,diplome_dpt)
                       dispense
                       year
                       cursus_map
                   in
                   state,
                   (
                     cursus_map,
                     addmap
                       (diplome_key,diplome_dpt)
                       (is_m2,diplome_dpt_en,diplome_label,diplome_label_en,elt) course_map)
                 | None ->
                   Remanent_state.warn_dft
                   __POS__
                   "The code of a course is missing"
                   Exit
                   (cursus_map,course_map)
                   state
            )
            (state, (cursus_map,StringOptMap.empty))
            filtered_classes
           in
           let state, decision_list =
             Remanent_state.get_decision_list
               ~firstname
               ~lastname
               ~year
               state
           in
           let state, cursus_map, split_cours =
             List.fold_left
               (fun (state, cursus_map,split_cours) decision ->
                  let diplome_key =
                    Some (Special_char.lowercase
                            (String.trim decision.Public_data.decision_program))
                  in
                  let diplome_dpt =
                    decision.Public_data.decision_dpt
                  in
                  let state, diplome_dpt =
                    dpt_of_acro who __POS__ state diplome_dpt origine
                  in
                  let state, b =
                    match diplome_dpt
                    with
                    | None -> state, true
                    | Some diplome_dpt ->
                      StringOptMap.fold
                        (fun (key,dpt) _ (state,b) ->
                           state,
                           (diplome_key,
                            diplome_dpt)=(key,dpt) || b
                        )
                      split_cours (state,false)
                  in
                  if b
                  then
                    state, cursus_map, split_cours
                  else
                    match diplome_dpt with
                    | None -> state, cursus_map, split_cours
                    | Some diplome_dpt ->
                      let state, cursus_map =
                        addfirstlast
                          state
                          (diplome_key,
                           diplome_dpt)
                          false
                          year
                          cursus_map
                      in
                      let split_cours =
                        StringOptMap.add
                          (diplome_key,
                           diplome_dpt)
                          [] split_cours
                      in
                      state, cursus_map, split_cours
               )
               (state,cursus_map,split_cours) decision_list
           in

        state, cursus_map, (year,situation,split_cours)::l)
        (state, StringOptMap.empty, [])
        l_rev
    in
    let print_foot_note string dpt x year y state =
        let dpt' =
          match dpt, string with
        | _, (None |  Some "dens") -> "dens"
        |_, _ -> Public_data.string_of_dpt dpt
        in
        Remanent_state.warn
            __POS__
            (Format.sprintf "FOOT %s %s %s %s %s" (match string with None -> "none" | Some x -> x) dpt' x year y) Exit state,
        String.trim y <> "" &&
        (match StringOptMap.find_opt (string,dpt') cursus_map with
        | None
        | Some(_,None) -> false
        | Some (_,Some y') -> year=y')
    in
    let l =
      match l with
      | [] ->
        [current_year, empty_bilan_annuel, StringOptMap.empty]
      | _ -> l
    in
    let state, l =
      match repartition with
      | Public_data.Annee_de_validation_du_cours -> state, l
      | Public_data.Annee_obtention_du_diplome ->
        begin
          let map =
            List.fold_left
              (fun map (year,situation,_) ->
                 Public_data.YearMap.add year
                   (year,situation,StringOptMap.empty)
                   map)
              Public_data.YearMap.empty
              l
          in
          let state, l =
            List.fold_left
              (fun
                (state, map)
                (year,_,split_cours) ->
                 StringOptMap.fold
                   (fun key course (state, map) ->
                      let state, year =
                        get_display_year
                          __POS__ "last year should be defined"
                          state year key cursus_map
                      in
                      add_course
                        __POS__ "diploma is not defined"
                        state year key course map
                   )
                   split_cours
                   (state,map))
              (state,map)
              l
          in
          let l =
            Public_data.YearMap.fold
              (fun _ x l -> x::l)
              l
              []
          in
          let l = List.rev l in
          state, l
        end
    in
    let state,mean,dens,natt, is_l3, cours_list, stage_list =
      List.fold_left
        (fun (state,mean,dens,natt,is_l3, cours_list, stage_list )
          (year,situation,split_cours) ->
           let gpscodelist = situation.gpscodelist in
           let who =
             Format.sprintf "%s in %s" who year
           in
           let state, tuteur =
             Remanent_state.get_mentoring
               ~year
               ~lastname
               ~firstname
               state
           in
           let state, tuteurs_secondaires =
             Remanent_state.get_mentoring_list
               ~year
               ~lastname
               ~firstname
               state
           in
           let tuteurs_secondaires =
             List.filter
               (fun p -> p.Public_data.secondaire <> None)
               tuteurs_secondaires
           in
           let state, tuteur =
             match tuteur with
             | Some t -> state, Some t
             | None ->
               begin
                 match Remanent_state.get_main_dpt state with
                 | state, (Public_data.DRI | Public_data.DI | Public_data.ENS) -> state, None
                 | state, (Public_data.ARTS
                          | Public_data.ECO
                          | Public_data.DMA
                          | Public_data.PHYS
                          | Public_data.IBENS
                          | Public_data.LILA)->
                   begin
                     match
                       gps_file.tuteur
                     with
                     | None -> state, None
                     | Some s ->
                       let t_genre, t_firstname, t_fullname =
                         Special_char.split_name s
                       in
                       let state, t_genre =
                         genre_opt_of_string_opt __POS__ state (Some t_genre);
                       in
                       state,
                       Some
                         {
                           Public_data.genre_du_tuteur= t_genre;
                           Public_data.nom_du_tuteur=Some t_fullname ;
                           Public_data.prenom_du_tuteur=Some t_firstname ;
                           Public_data.annee_academique=year;
                           Public_data.courriel_du_tuteur=None;
                           Public_data.nom_de_l_etudiant=lastname;
                           Public_data.prenom_de_l_etudiant=firstname;
                           Public_data.secondaire=None;
                   }
                 end

               end
           in
           let current_dpt =
             match
               situation.departement_principal
             with
             | Some a -> Public_data.dpt_of_string
                           (acro_of_gps_name a)
             | None -> Public_data.ENS
           in
           let state, tuteur =
             match tuteur with
             | None ->
               begin
               let state =
                 Remanent_state.add_missing_mentor
                   state
                   {
                     Public_data.missing_mentor_firstname=firstname;
                     Public_data.missing_mentor_lastname=lastname;
                     Public_data.missing_mentor_year=year;
                     Public_data.missing_mentor_promotion=promo;
                   }
               in
               state,
               ("","",1.)
             end

             | Some tuteur ->
               let state, genre =
                 match
                   gps_file.genre
                 with
                 | None ->
                   Remanent_state.warn
                     __POS__
                     "missing gender"
                     Exit
                     state, Public_data.Unknown
                 | Some a -> state, a
               in
               let state, genre_du_tuteur =
                 match
                   tuteur.Public_data.genre_du_tuteur
                 with
                 | None ->
                   Remanent_state.warn
                     __POS__
                     "missing mentor gender"
                     Exit
                     state, Public_data.Unknown
                 | Some a -> state, a
               in
               let state, nom_du_tuteur =
                 match
                   tuteur.Public_data.nom_du_tuteur
                 with
                 | None ->
                   Remanent_state.warn
                     __POS__
                     "missing mentor name"
                     Exit
                     state, ""
                 | Some a -> state, a
               in
               let state, prenom_du_tuteur =
                 match
                   tuteur.Public_data.prenom_du_tuteur
                 with
                 | None ->
                   Remanent_state.warn
                     __POS__
                     "missing mentor first name"
                     Exit
                     state, ""
                 | Some a -> state, a
               in
               let state =
                 if do_report report &&
                    (year <= current_year ||
                     need_a_mentor gps_file)
                 then
                   let state =
                     Remanent_state.add_mentor
                       state
                       {Public_data.mentor_attribution_year =
                          tuteur.Public_data.annee_academique;
                        Public_data.mentor_gender =
                          genre_du_tuteur;
                        Public_data.mentor_lastname =
                          nom_du_tuteur;
                        Public_data.mentor_firstname
                        =
                          prenom_du_tuteur;
                        Public_data.mentor_academic_year = year;
                        Public_data.mentor_student_promo =
                         promo ;
                        Public_data.mentor_student_gender =
                          genre;
                        Public_data.mentor_student_lastname =
                          lastname ;
                        Public_data.mentor_student_firstname =
                          firstname ;
                        Public_data.mentor_student_dpt =
                          current_dpt ;
                        Public_data.mentor_email =
                          Tools.unsome_string
                            tuteur.Public_data.courriel_du_tuteur ;
                        Public_data.mentor_secondary =
                           tuteur.Public_data.secondaire
                       }
                   in
                   state
                 else state
               in
               begin
                 match
                   tuteur.Public_data.nom_du_tuteur,           tuteur.Public_data.prenom_du_tuteur,
                   tuteur.Public_data.genre_du_tuteur,
                   tuteur.Public_data.courriel_du_tuteur
                 with
                 | None, (None | Some _),
                   (None | Some _), None ->
                   let msg =
                     Printf.sprintf
                       "Tuteur inconnu pour %s"
                       who
                   in
                   Remanent_state.warn_dft
                     __POS__
                     msg
                     Exit
                     ("","",1.)
                     state
                 | Some x, Some y, Some z, _ ->
                   state,
                   (Printf.sprintf
                      "%s %s %s"
                      (match z with
                       | Public_data.Masculin ->
                         "Tuteur : "
                       | Public_data.Feminin ->
                         "Tutrice : "
                       | Public_data.Unknown -> "")
                      (Special_char.capitalize y)
                      (Special_char.uppercase x),
                      Printf.sprintf
                         "Mentor: %s %s"
                         (Special_char.capitalize y)
                         (Special_char.uppercase x),
                    2./.3.
                   )
                 | None, _, _, Some x ->
                   state, (x, x, 2./.3.)
                 | Some x, _, _, _ ->
                   state, (x, x, 2./.3.)
               end
           in
           let state, tuteur_bis =
             begin
               match
                 tuteurs_secondaires
               with
               | [] -> state, None
               | tuteur::_ ->
                 begin
                   match
                     tuteur.Public_data.nom_du_tuteur,
                     tuteur.Public_data.prenom_du_tuteur,
                     tuteur.Public_data.genre_du_tuteur,
                     tuteur.Public_data.courriel_du_tuteur
                   with
                   | None, (None | Some _),
                     (None | Some _), None ->
                     let msg =
                       Printf.sprintf
                         "Tuteur secondaire inconnu pour %s"
                         who
                     in
                     Remanent_state.warn_dft
                       __POS__
                       msg
                       Exit
                       (Some ("","",1.))
                       state
                   | Some x, Some y, Some z, _ ->
                     state,
                     Some (Printf.sprintf
                        "%s %s %s"
                        (match z with
                         | Public_data.Masculin ->
                           "Tuteur (secondaire): "
                         | Public_data.Feminin ->
                           "Tutrice (secondaire): "
                         | Public_data.Unknown -> "")
                        (Special_char.capitalize y)
                        (Special_char.uppercase x),
                        Printf.sprintf
                           "Secondary mentor: %s %s"
                           (Special_char.capitalize y)
                           (Special_char.uppercase x),
                      2./.3.
                     )
                   | None, _, _, Some x ->
                     state, Some (x, x, 2./.3.)
                   | Some x, _, _, _ ->
                     state, Some (x, x, 2./.3.)
                 end
             end
           in
           let state =
             if do_report report &&
                year <= current_year
             then
               List.fold_left
                 (fun state tuteur ->
                   let state, genre =
                     match
                       gps_file.genre
                     with
                     | None ->
                       Remanent_state.warn
                         __POS__
                         "missing gender"
                         Exit
                         state, Public_data.Unknown
                     | Some a -> state, a
                   in
                   let state, genre_du_tuteur =
                     match
                       tuteur.Public_data.genre_du_tuteur
                     with
                     | None ->
                       Remanent_state.warn
                         __POS__
                         "missing mentor gender"
                         Exit
                         state, Public_data.Unknown
                     | Some a -> state, a
                   in
                   let state, nom_du_tuteur =
                     match
                       tuteur.Public_data.nom_du_tuteur
                     with
                     | None ->
                       Remanent_state.warn
                         __POS__
                         "missing mentor name"
                         Exit
                         state, ""
                     | Some a -> state, a
                   in
                   let state, prenom_du_tuteur =
                     match
                       tuteur.Public_data.prenom_du_tuteur
                     with
                     | None ->
                       Remanent_state.warn
                         __POS__
                         (Format.sprintf
                            "missing mentor first name for %s"
                            who)
                         Exit
                         state, ""
                     | Some a -> state, a
                   in
                   Remanent_state.add_mentor
                      state
                      {Public_data.mentor_attribution_year =
                         tuteur.Public_data.annee_academique;
                       Public_data.mentor_gender =
                         genre_du_tuteur;
                       Public_data.mentor_lastname =
                         nom_du_tuteur;
                       Public_data.mentor_firstname
                       =
                         prenom_du_tuteur;
                       Public_data.mentor_academic_year = year;
                       Public_data.mentor_student_promo =
                         promo ;
                       Public_data.mentor_student_gender =
                         genre;
                       Public_data.mentor_student_lastname =
                         lastname ;
                       Public_data.mentor_student_firstname =
                         firstname ;
                       Public_data.mentor_student_dpt =
                         current_dpt ;
                       Public_data.mentor_email =
                         Tools.unsome_string
                           tuteur.Public_data.courriel_du_tuteur ;
                       Public_data.mentor_secondary =
                           tuteur.Public_data.secondaire
                      })
                 state tuteurs_secondaires
             else state
           in

           if year > current_year then
               state,mean,dens,natt,is_l3,cours_list, stage_list
           else
             let l =
               [21.0;11.67;48.33;26.67;7.3;10.00;5.17]
             in
             let sum =
               List.fold_left
                 (fun total a -> total+.a)
                 0. l
             in
             let state, admission_opt =
               let year' = next_year year in
               begin
                 match year' with
                 | Some year ->
                   Remanent_state.get_admission
                     ~firstname
                     ~lastname
                     ~year
                     state
                 | None ->
                   let msg =
                     Format.sprintf
                       "Illegal year %s for %s (admission)"
                       year who
                   in
                   let state =
                     Remanent_state.warn
                       __POS__
                       msg
                       Exit
                       state
                   in
                   state, None
               end
             in
             let nprogram =
               StringOptMap.cardinal split_cours
             in
             let _, dens_pos =
               StringOptMap.fold
                 (fun (string,_) _ (i,opt) ->
                    match string with
                    | Some "dens" -> (i+1,Some i)
                    | _ -> (i+1,opt)
                 )
                 split_cours
                 (1,None)
             in
             let size =
               List.rev_map
                 (fun a -> Some (a/.(sum*.1.12)))
                 (List.rev l)
             in
             (*   let situation' = situation in*)
             if StringOptMap.is_empty split_cours
             then
               let suite = false in
               let state, is_l3' =
                 heading
                   ~who ~firstname ~lastname
                   ~promo ~origine
                   ~year ~situation ~gpscodelist
                   ~tuteur ?tuteur_bis
                   cursus_map split_cours
                   picture_list suite gps_file state
               in
               let () =
                 Remanent_state.fprintf
                   state "\n\ \\vfill\n\ \n\ "
               in
               let state =
                 foot signature state
               in
               let () =
                 Remanent_state.fprintf
                   state "\n\ \\vfill\n\ \n\ "
               in
               let () =
                   Remanent_state.fprintf
                     state "\\pagebreak\n\ "
               in
               state, mean, dens, natt, is_l3 || is_l3', cours_list, stage_list
             else
               begin
                 let _, state, mean, dens, natt, is_l3, cours_list, stage_list  =
                   StringOptMap.fold
                     (fun
                       (string,dpt)  list
                       (i,state,mean,dens,natt, is_l3,cours_list,stage_list)
                       ->
                         let is_m2 =
                              match list with [] -> false
                                            | (b,_,_,_,_)::_ -> b
                         in
                         let state, is_l3' =
                           if i mod 2 = 1
                           then
                             let suite = i<>1 in
                             let state, is_l3 =
                               heading
                                 ~who ~firstname ~lastname
                                 ~promo ~origine ~gpscodelist
                                 ~year ~situation
                                 ~tuteur ?tuteur_bis
                                 cursus_map split_cours
                                 picture_list suite gps_file state
                             in
                             let () =
                               Remanent_state.fprintf
                                 state "\n\ \\vfill\n\ \n\ "
                             in
                             state, is_l3
                           else state, is_l3
                         in
                         let
                           (state,mean,dens,natt,cours_list,stage_list)
                           =
                           program
                             ~is_m2 ~print_foot_note
                             ~origine ~string ~dpt:(Public_data.dpt_of_string dpt) ~year ~who ~gpscodelist
                             ~alloc_suffix ~mean ~cours_list ~stage_list
                             ~firstname ~lastname ~promo ~cursus_map ~size
                             ~stages ~current_year (*~report
                             ~keep_success ~keep_faillure*)
                             ~dens
                             ~natt
                              list state
                         in
                         let () =
                           Remanent_state.fprintf
                             state "\n\ \\vfill\n\ \n\ "
                         in
                         let () =
                           if
                             begin
                               match
                                 dens_pos
                               with
                               | None -> i = nprogram
                               | Some 1 -> true
                               | Some p -> i = p-1
                             end
                           then
                             match admission_opt with
                             | None -> ()
                             | Some admission ->
                               let lineproportion = 1. in
                               let english =
                                 match admission.Public_data.admission_decision_en with
                                 | None -> None
                                 | Some x ->
                                   Some (Format.sprintf "\\textbf{%s}" x)
                               in
                               let () =
                                 Remanent_state.log_string
                                   ~lineproportion
                                   ?english
                                   state
                                   (Format.sprintf "\\textbf{%s}"
                                      admission.Public_data.admission_decision)
                               in
                               let () =
                                 Remanent_state.fprintf
                                   state "\n\ \\vfill\n\ \n\ "
                               in
                               ()
                         in
                         let state =
                           if i mod 2 = 0 || i = nprogram
                           then
                             let state =
                               foot signature state
                             in
                             let () =
                               Remanent_state.fprintf
                                 state "\n\ \\vfill\n\ \n\ "
                             in
                             state
                           else
                             state
                         in
                         let () =
                           if i mod 2 = 0 || i = nprogram
                           then
                             Remanent_state.fprintf
                               state "\\pagebreak\n\ "
                         in
                         (i+1,state,mean,dens,natt,is_l3 || is_l3',cours_list,stage_list)
                     )
                     split_cours
                     (1,state,mean,dens,natt,false,cours_list,stage_list)
                 in
                 state,mean,dens,natt, is_l3, cours_list, stage_list
               end
        )
        (state,mean_init,dens_init,n_att_init, false,cours_list_init,stage_list_init)
        l
    in
    let _ = natt in
    let dens_total, dens_total_potential, mandatory, math, math_math_info =
      Public_data.YearMap.fold
        (fun _ (t,pt,m,ma,mi) (t',pt',m',ma',mi') -> t+.t',pt+.pt',m+m',ma+ma',mi+mi')
        dens
        (0.,0.,0,0,0)
    in
    let state, p, com_year  =
      match Remanent_state.get_commission state
      with
      | state, Some (_, a) ->
        state, (fun y -> y<=a), Some a
      | state, None ->
        let state, y = Remanent_state.get_current_academic_year state in
        state, (fun _ -> true), Some y
    in
    let dens_year, dens_year_potential,_,_,_ =
      match com_year with
      | None -> (0.,0.,0,0,0)
      | Some com_year ->
        match
          Public_data.YearMap.find_opt
            com_year
            dens
        with
        | Some a -> a
        | None -> (0.,0.,0,0,0)
    in
    let state, m2_list, dip_autre_list =
      Public_data.YearMap.fold
        (fun year situation (state, m2_list, dip_autre_list) ->
      (*if not (year = current_year) then (state, m2_list, dip_autre_list) else*)
    let state,m2_list,dip_autre_list =
        begin
        let gpscodelist = situation.gpscodelist in
        let state, list =
          Remanent_state.get_dispenses
            ~firstname
            ~lastname
            ~year:current_year
            state
        in
        let key =
          if lpoly situation then Some "Bachelor_de_l_X"
          else if lpe origine then Some "Pensionnaires_etrangers"
          else if lerasmus origine then Some "Erasmus"
          else if lechange_dri situation then Some "Échange DRI"
          else if list <> [] then Some "Dispenses"
          else
            None
        in
        let state =
          match key with
          | None -> state
          | Some key ->
            let input_rep,file_name = rep, snd output in
            let file_name = Copy.pdf_file file_name in
            match Remanent_state.get_commission state with
            | state, None -> state
            | state, Some _ ->
              if
                 keep_success
              then
                match
                    Remanent_state.get_commission_rep_from_key
                      key
                      state
                with
                | state, (_,_,output_rep) ->
                  let state =
                    if lpoly situation
                    || lerasmus origine || lpe origine
                    || lechange_dri situation
                    || list <> []
                    then
                      Remanent_state.push_copy
                        ~input_rep
                        ~file_name
                        ~output_rep
                        state
                    else
                      state
                  in
                  state
              else state
        in
        let list_national_diploma = snd mean in
        let state,m2_list,dip_autre_list =
          List.fold_left
            (fun
                (state,m2_list,dip_autre_list)
                (key, moyenne_opt, mention_opt, validated_opt, val_year)
              ->
               if not (year = string_of_int val_year) then (state,m2_list,dip_autre_list)
               else
               match
                 StringOptMap.find_opt key (fst mean)
               with
               | None -> state,m2_list,dip_autre_list
               | Some (is_m2,_,l,_) ->
                 let state, total, ects_qui_comptent, ects =
                   List.fold_left
                     (fun (state, total, ects_qui_comptent,
                           ects) data  ->
                       match data with
                       | _, None -> state, total,
                                    ects_qui_comptent, ects
                       | Some Public_data.Float f, Some
                           cours_ects ->
                         state,
                         total+.f*.cours_ects,
                         ects_qui_comptent+.cours_ects,
                         ects+.cours_ects
                      | Some (Public_data.Valide_sans_note | Public_data.String _) ,
                         Some cours_ects  ->
                         state, total, ects_qui_comptent,
                         ects+.cours_ects
                       | (None, _)
                       | (Some (Public_data.Temporary _ | Public_data.Abandon | Public_data.En_cours | Public_data.Absent),_) ->
                         (Remanent_state.warn
                            __POS__
                            "Incompatible grade"
                            Exit
                            state),
                         total, ects_qui_comptent, ects
                     )
                     (state, 0., 0., 0.)
                     l
                 in
                 let mean =
                   if ects < 60.
                   then
                     None
                   else
                     Some (total /. ects_qui_comptent)
                 in
                 let mean =
                   match
                     moyenne_opt
                   with
                   | None -> mean
                   | Some _ -> moyenne_opt
                 in
                 let mention =
                   match mean with
                   | None -> None
                   | Some mean ->
                     if mean <12. then Some ""
                     else if mean <14. then Some "Assez Bien"
                     else if mean <16. then Some "Bien"
                     else Some "Très bien";
                 in
                 let mention =
                   match
                     mention_opt
                   with
                   | None -> mention
                   | (Some _) -> mention_opt
                 in
                 let validated =
                   match validated_opt with
                   | Some v -> v
                   | None ->
                     match mean with
                     | None -> false
                     | Some mean -> mean >= 10.
                 in
                 let state, d_nat =
                   let input_rep,file_name = rep, snd output in
                   let file_name = Copy.pdf_file file_name in
                   let y = string_of_int val_year in
                   match Remanent_state.get_commission state with
                   | state, None -> state, false
                   | state, Some (_,i) ->
                     let state, dpt = Remanent_state.get_main_dpt state in
                     if i=y &&
                        (dpt = Public_data.dpt_of_string (snd key))
                         &&
                         ((validated && keep_success)
                          || ((not validated) && keep_faillure))
                     then
                       let state, output_rep =
                         if lpoly situation then
                           state, "Bachelor_de_l_X"
                         else if lerasmus origine then
                           state, "Erasmus"
                         else if lpe origine then
                           state, "Pensionnaires_etrangers"
                         else if lechange_dri situation then
                           state, "Échange DRI"
                         else
                           match
                             let state, key =
                               match fst key with
                                | None -> state, ""
                                | Some "l" ->
                                  if
                                    lmath ~year:current_year ~firstname ~lastname situation state && linfo situation
                                  then state, "L3_mathinfo"
                                  else if lmathphys situation
                                  then state, "L3_mathphys"
                                  else state, "L3"
                                | Some "m" -> state, "M1"
                                | Some a ->
                                    state,
                                    a
                             in
                             Remanent_state.get_commission_rep_from_key
                               key
                               state
                           with
                           | state, (_,_,rep') ->
                             let suf =
                               if validated then
                                 "ras"
                               else
                                 "a_discuter"
                             in
                             let output_rep =
                               match rep',suf with
                               | a,"" | "",a -> a
                               | a,b -> Printf.sprintf "%s/%s"  a b
                             in
                             state, output_rep
                       in
                       let state =
                         if good key || lpoly situation
                            || lerasmus origine || lpe origine
                            || lechange_dri situation
                         then
                         let diplome_dpt = Public_data.dpt_of_string (snd key) in
                         let diplome_niveau =
                            (match fst key with
                                | None -> ""
                                | Some a -> a)
                         in
                         let diplome_year = string_of_int val_year in

                         let state, _, cursus =
                            Univ.get_univ
                              ~diplome_dpt ~diplome_niveau ~diplome_year ~firstname ~lastname
                              gpscodelist state
                         in

                         let cursus =
                           match cursus with
                           | Some cursus -> cursus
                           | _ -> Public_data.empty_cursus
                         in
                            if is_l3
                             && cursus.Public_data.cursus_gps = None
                             && cursus.Public_data.cursus_niveau = "m"
                            then state
                            else
                           Remanent_state.push_copy
                               ~input_rep
                               ~file_name
                               ~output_rep
                               state
                           else
                             state
                         in
                         state, true
                     else
                       state, false
                 in
                 let diplome_dpt = Public_data.dpt_of_string (snd key) in
                 let diplome_niveau =
                    (match fst key with
                        | None -> ""
                        | Some a -> a)
                 in
                 let diplome_year = string_of_int val_year in
                 let state, univ, cursus =
                    Univ.get_univ
                      ~diplome_dpt ~diplome_niveau ~diplome_year ~firstname ~lastname
                      gpscodelist state
                 in
                 let univ =
                    match univ with
                     | Some univ -> univ
                     | _ -> Public_data.Upartenaire
                 in
                 let cursus =
                   match cursus with
                   | Some cursus -> cursus
                   | _ -> Public_data.empty_cursus
                 in
                 let dpl =
                 {
                   Public_data.diplome_dpt;
                   Public_data.diplome_niveau ;
                   Public_data.diplome_univ_key = univ ;
                   Public_data.diplome_cursus = cursus ;
                   Public_data.diplome_ranking = None ;
                   Public_data.diplome_effectif = None ;
                   Public_data.diplome_origine = origine;
                   Public_data.diplome_statut = gps_file.statut ;
                   Public_data.diplome_firstname = firstname ;
                   Public_data.diplome_lastname = lastname ;
                   Public_data.diplome_gender =
                     begin
                       match gps_file.genre
                       with
                       | Some a -> a
                       | None -> Public_data.Unknown
                       end ;
                   Public_data.diplome_promotion = promo;
                   Public_data.diplome_nb_ects = ects ;
                   Public_data.diplome_moyenne = mean ;
                   Public_data.diplome_year;
                   Public_data.diplome_mention = mention;
                   Public_data.diplome_recu = validated ;
                   Public_data.diplome_commission = d_nat ;
               }
               in
               if (not validated) || (is_l3
                  && cursus.Public_data.cursus_gps = None
                  && cursus.Public_data.cursus_niveau = "m")
                  ||  match StringOptMap.find_opt
                      key
                      cursus_map with | Some (_,Some fin) -> not (fin = diplome_year) | _ -> true
               then state, m2_list, dip_autre_list
               else
                 let state, m2_list, dip_autre_list =
                    if validated then
                        if is_m2 then state, dpl::m2_list, dip_autre_list
                        else state, m2_list, dpl::dip_autre_list
                    else state, m2_list, dip_autre_list
                 in
                 if not (do_report report)
                 then state, m2_list, dip_autre_list
                 else
                       match com_year with
                       | Some com_year when com_year = current_year ->
                      Remanent_state.add_national_diploma state dpl,
                      m2_list, dip_autre_list
                      | None  | Some _ ->
                          state,m2_list, dip_autre_list

            )
            (state,m2_list,dip_autre_list)
            list_national_diploma
        in
        state, m2_list, dip_autre_list
      end
      in state, m2_list, dip_autre_list)
      gps_file.situation (state, m2_init, dip_autre_list_init)

    in
    let state, main_dpt =
      Remanent_state.get_main_dpt state
    in
    let current_dpt = main_dpt in
    let cours_a_trier = cours_list in
    let stages_a_trier = stage_list in
    let n_inscription =
      Public_data.YearMap.fold
        (fun year bilan n_inscription ->
           if p year
           then
             match bilan.inscription_au_DENS with
             | Some true -> n_inscription + 1
             | Some false | None -> n_inscription
           else n_inscription)
        gps_file.situation
        0
    in
    let dens =
        {
          Public_data.dens_main_dpt = main_dpt ;
          Public_data.dens_firstname = firstname ;
          Public_data.dens_lastname = lastname;
          Public_data.dens_promotion = promo;
          Public_data.dens_total_ects = dens_total ;
          Public_data.dens_current_year_ects =
            dens_year ;
          Public_data.dens_total_potential_ects =
            dens_total_potential ;
          Public_data.dens_current_year_potential_ects = dens_year_potential ;
          Public_data.dens_nb_inscriptions = n_inscription ;
          Public_data.dens_nb_mandatory_course = mandatory ;
          Public_data.dens_nb_math_course = math ;
          Public_data.dens_nb_math_and_math_info_course = math_math_info ;
          Public_data.dens_sortant=false;
          Public_data.dens_derogation=false;
          Public_data.dens_master=m2_list;
          Public_data.dens_parcours=dip_autre_list;
          Public_data.dens_cours_a_trier= cours_a_trier;
          Public_data.dens_cours_discipline_principale=Public_data.empty_repartition_diplomes;
          Public_data.dens_cours_hors_disciplines_principale=Public_data.empty_repartition_diplomes; Public_data.dens_cours_langue=[];
          Public_data.dens_cours_mineure=Public_data.StringMap.empty;
          Public_data.dens_cours_majeure=Public_data.StringMap.empty;
          Public_data.dens_cours_activite=[];
          Public_data.dens_activite_a_trier=stages_a_trier;
          Public_data.dens_activite_recherche=[];
          Public_data.dens_activite_internationale=[];
          Public_data.dens_activite_autre=[];
          Public_data.dens_cours_par_dpt = Public_data.StringMap.empty;
        }
  in
  let state, dens = Dens.split_courses dens state in
  let state, dens = Dens.split_stages dens state in
  let state, dens = Dens.collect_mineure dens state in
  let state, tuteur =
    Remanent_state.get_mentoring
      ~year:current_year
      ~lastname
      ~firstname
      state
  in
  let state, tuteurs_secondaires =
    Remanent_state.get_mentoring_list
      ~year:current_year
      ~lastname
      ~firstname
      state
  in
  let tuteurs_secondaires =
    List.filter
      (fun p -> p.Public_data.secondaire <> None)
      tuteurs_secondaires
  in
  let state, tuteur =
    match tuteur with
    | Some t -> state, Some t
    | None ->
      begin
        match Remanent_state.get_main_dpt state with
        | state, (Public_data.DRI | Public_data.DI | Public_data.ENS) -> state, None
        | state, (Public_data.ARTS
                 | Public_data.ECO
                 | Public_data.DMA
                 | Public_data.PHYS
                 | Public_data.IBENS
                 | Public_data.LILA)->
          begin
            match
              gps_file.tuteur
            with
            | None -> state, None
            | Some s ->
              let t_genre, t_firstname, t_fullname =
                Special_char.split_name s
              in
              let state, t_genre =
                genre_opt_of_string_opt __POS__ state (Some t_genre);
              in
              state,
              Some
                {
                  Public_data.genre_du_tuteur= t_genre;
                  Public_data.nom_du_tuteur=Some t_fullname ;
                  Public_data.prenom_du_tuteur=Some t_firstname ;
                  Public_data.annee_academique=current_year;
                  Public_data.courriel_du_tuteur=None;
                  Public_data.nom_de_l_etudiant=lastname;
                  Public_data.prenom_de_l_etudiant=firstname;
                  Public_data.secondaire=None;
          }
        end

      end
  in
  let tuteur = match tuteur with Some a -> a | None -> Public_data.empty_tutorat in
  let state, tuteur =
  begin
    match
      tuteur.Public_data.nom_du_tuteur,           tuteur.Public_data.prenom_du_tuteur,
      tuteur.Public_data.genre_du_tuteur,
      tuteur.Public_data.courriel_du_tuteur
    with
    | None, (None | Some _),
      (None | Some _), None ->
      let msg =
        Printf.sprintf
          "Tuteur inconnu pour %s"
          who
      in
      Remanent_state.warn_dft
        __POS__
        msg
        Exit
        ("","",1.)
        state
    | Some x, Some y, Some z, _ ->
      state,
      (Printf.sprintf
         "%s %s %s"
         (match z with
          | Public_data.Masculin ->
            "Tuteur : "
          | Public_data.Feminin ->
            "Tutrice : "
          | Public_data.Unknown -> "")
         (Special_char.capitalize y)
         (Special_char.uppercase x),
         Printf.sprintf
            "Mentor: %s %s"
            (Special_char.capitalize y)
            (Special_char.uppercase x),
       2./.3.
      )
    | None, _, _, Some x ->
      state, (x, x, 2./.3.)
    | Some x, _, _, _ ->
      state, (x, x, 2./.3.)
  end
in
let state, tuteur_bis =
begin
  match
    tuteurs_secondaires
  with
  | [] -> state, None
  | tuteur::_ ->
    begin
      match
        tuteur.Public_data.nom_du_tuteur,
        tuteur.Public_data.prenom_du_tuteur,
        tuteur.Public_data.genre_du_tuteur,
        tuteur.Public_data.courriel_du_tuteur
      with
      | None, (None | Some _),
        (None | Some _), None ->
        let msg =
          Printf.sprintf
            "Tuteur secondaire inconnu pour %s"
            who
        in
        Remanent_state.warn_dft
          __POS__
          msg
          Exit
          (Some ("","",1.))
          state
      | Some x, Some y, Some z, _ ->
        state,
        Some (Printf.sprintf
           "%s %s %s"
           (match z with
            | Public_data.Masculin ->
              "Tuteur (secondaire): "
            | Public_data.Feminin ->
              "Tutrice (secondaire): "
            | Public_data.Unknown -> "")
           (Special_char.capitalize y)
           (Special_char.uppercase x),
           Printf.sprintf
              "Secondary mentor: %s %s"
              (Special_char.capitalize y)
              (Special_char.uppercase x),
         2./.3.
        )
      | None, _, _, Some x ->
        state, Some (x, x, 2./.3.)
      | Some x, _, _, _ ->
        state, Some (x, x, 2./.3.)
    end
end
in
let state,year = Remanent_state.get_current_academic_year state in
  let state,_ =
      heading
        ~who ~firstname ~lastname
        ~promo ~origine ~dens:true
        ~year   ~gpscodelist:[]
        ~tuteur ?tuteur_bis
        cursus_map StringOptMap.empty
        picture_list false gps_file ~situation:empty_bilan_annuel state
  in
  let state = Dens.dump_dens dens state in
  let state =
          if
            main_dpt = current_dpt
            &&
            do_report report &&
             (n_inscription > 0 || dens_total_potential > 0.
              || dens_total > 0.)
          then
                  Remanent_state.add_dens
              state
              dens
          else
            state
        in
    let state =
      if do_report report &&
         (match gps_file.annee_en_cours with
          | Some a -> a < current_year
          | None -> false) &&
          (need_a_mentor gps_file)
      then
        let state, tuteurs =
          Remanent_state.get_mentoring_list
            ~year:current_year
            ~lastname
            ~firstname
            state
        in
        let state, tuteur =
          Remanent_state.get_mentoring
            ~year:current_year
            ~lastname
            ~firstname
            state
        in
        let tuteurs =
          match tuteur with
          | None -> tuteurs
          | Some tuteur ->
            if List.mem tuteur tuteurs
            then tuteurs
            else tuteur::tuteurs
        in
        List.fold_left
          (fun state tuteur ->
             if
               tuteur.Public_data.annee_academique =
               current_year
             then
               Remanent_state.add_mentor
                 state
                 {Public_data.mentor_attribution_year =
                    tuteur.Public_data.annee_academique;
                  Public_data.mentor_gender =
                    (match tuteur.Public_data.genre_du_tuteur
                     with Some a -> a
                        | None -> Public_data.Unknown);
                  Public_data.mentor_lastname =
                    Tools.unsome_string
                      tuteur.Public_data.nom_du_tuteur;
                  Public_data.mentor_firstname
                  =
                    Tools.unsome_string
                      tuteur.Public_data.prenom_du_tuteur
                 ;
                  Public_data.mentor_academic_year =
                    current_year;
                  Public_data.mentor_student_promo = promo ;
                  Public_data.mentor_student_gender =
                    (match gps_file.genre
                     with Some a -> a
                        | None -> Public_data.Unknown);
                  Public_data.mentor_student_lastname =
                    lastname ;
                  Public_data.mentor_student_firstname =
                    firstname ;
                  Public_data.mentor_student_dpt =
                    main_dpt ;
                  Public_data.mentor_email =
                    Tools.unsome_string
                      tuteur.Public_data.courriel_du_tuteur ;
                  Public_data.mentor_secondary =
                      tuteur.Public_data.secondaire ;
                 } else state)
          state tuteurs
      else state
    in
    let state = Remanent_state.close_logger state in
    let state =
      Remanent_state.restore_std_logger state old_logger
    in
    state, Some (rep, snd output)
