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
let dpt_info_gps_name = dpt_info
let dpt_phys_gps_name = dpt_phys
let dpt_maths_gps_name = "mathematiques et applications"
let acro_dpt_phys = "PHYS"
let acro_dpt_info = "DI"
let acro_dpt_maths = "DMA"
let dpt_info_full = "Département d'Informatique"
let dpt_maths_full = "Département de Mathématiques et Applications"
let dpt_phys_full = "Département de Physique"

let simplify_string s =
  Special_char.lowercase
    (Special_char.correct_string_txt
       (String.trim s))

let addmap x data map =
  let old =
    match
      StringOptMap.find_opt x map
    with
    | Some x -> x
    | None -> []
  in
  StringOptMap.add x (data::old) map

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
      let state, s =
        Notes.to_string
          __POS__ state a
      in
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
         | Public_data.Feminin -> "F")
    in state

let log_statut
        state (label, string_opt) =
      match string_opt with
      | None -> state
      | Some a ->
        let () =
          Remanent_state.log
            state "%s: %s" label
            (match a with
             | Public_data.Eleve_bis -> "Eleve BIS"
             | Public_data.Eleve -> "Eleve"
             | Public_data.Etudiant -> "Etudiant"
             | Public_data.Ex_eleve -> "Ancien eleve"
             | Public_data.Ex_etudiant -> "Ancien etudiant"
             | Public_data.Boursier_si ->
               "Boursier - selection internationale")
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

type stage =
  {
    periode: string option;
    sujet: string option;
    directeur_de_stage: string option;
    responsable_local: string option;
    service_labo_dpt: string option;
    etablissement_ou_entreprise: string option;
    stage_credits: float option;
    stage_valide: bool option;
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
  let state =
    List.fold_left
      log_bool
      state
      [
        "validé", stage.stage_valide;
        "accord", stage.stage_accord;
      ]
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
  }

type gps_file =
  {
    nom: string option;
    prenom: string option;
    nom_complet: string option;
    genre: Public_data.genre option ;
    date_de_naissance: string option;
    promotion: string option;
    origine: string option;
    statut: Public_data.statut option;
    annee_en_cours: Public_data.annee option;
    contact_ens: string option;
    tuteur: string option;
    situation: bilan_annuel Public_data.YearMap.t;
    stages: stage list;
  }

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
      ("statut",gps.genre)
  in
  let state =
    List.fold_left
      log_string
      state
      [
        "nom complet",gps.nom_complet;
        "date de naissance",gps.date_de_naissance;
        "promotion",gps.promotion;
        "origine",gps.origine
      ]
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
   prevalide: Public_data.valide option;
   precode: string option;
  }

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
       let state, note =
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
         | Some v , Some n ->
           begin
             let state, note_opt = Notes.of_string __POS__ state n in
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
               if Notes.valide note = Valide.valide v
               then
                 state, note_opt
               else
                 let state, note_string =
                   Notes.to_string
                     __POS__
                     state
                     note
                 in
                 let state, v_string =
                   Valide.to_string
                     __POS__
                     state
                     v
                 in
                 let msg =
                   Format.sprintf
                     "Note %s and validity status %s are incompatible for %s"
                     note_string v_string who
                 in
                 Remanent_state.warn_dft
                   __POS__
                   msg
                   Exit
                   note_opt
                   state
           end
         | None , Some n ->
           begin
             let state, note_opt = Notes.of_string __POS__ state n in
             let msg =
               Format.sprintf
                 "Undefined validity status for note %s for %s"
                 n who
             in
             Remanent_state.warn_dft
                 pos
                 msg
                 Exit
                 note_opt
                 state
           end
         | None, None ->
           state, Some Public_data.En_cours
       in
       let code_cours = remanent.precode in
       state, {remanent.rem_cours with note ; code_cours})
    (fun state bilan cours -> state, {bilan with cours})

let store_stage =
  store_gen
    (fun state _bilan -> state, [])
    (fun state pos ~who _remanent ->
       let _ = pos, who in
       state, [])
    (fun state bilan _stages -> state, bilan)

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
    pos state annee remanent
  =
  let state, remanent =
    match
      annee
    with
    | None ->
      Remanent_state.warn_dft
        pos
        "Current year is not documented"
        Exit
        remanent
        state
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
          s ["m";"mr";"monsieur";"m.";"mr"]
      then
          state, Some Public_data.Masculin
      else if
        List.mem
          s
          ["mlle";"mme";"madame";"mademoiselle"]
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
    Public_data.Boursier_si,["boursier si"]
  ]

let statut_opt_of_string_opt pos state s_opt =
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
          let msg =
            Format.sprintf
              "Ill-formed Statut (%s)"
              s
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
      aux state statuts

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
      lift_gps
        (fun origine gps_file ->
           {gps_file with origine});
      Public_data.Statut,
      lift_gps_state
        (fun state genre gps_file ->
           let state, statut =
             statut_opt_of_string_opt __POS__ state genre
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
      set_dens __POS__;
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
      lift_cours_state
        (fun state data cours ->
           let state, accord =
           bool_opt_of_string_opt __POS__ state data
           in
           state, {cours with accord});
      Public_data.Valide,
        (fun state data remanent ->
           match data with
           | None -> state, remanent
           | Some data ->
             let state, prevalide =
               Valide.of_string __POS__ state data
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
      lift_cours
        (fun com_opt cours ->
           match com_opt with
           | None -> cours
           | Some com ->
             let commentaire =
               com::cours.commentaire
             in
             {cours with commentaire}
        );
      Public_data.Stages_et_Sejours_a_l_Etranger,fun_default;
      Public_data.Periode,
      lift_stage
        (fun periode stage -> {stage with periode});
      Public_data.Sujet_du_Stage_Type_du_Sejour,
      lift_stage
        (fun sujet stage -> {stage with sujet});
      Public_data.Directeur_de_Stage,
      lift_stage
        (fun directeur_de_stage stage -> {stage with directeur_de_stage});
      Public_data.Responsable_local,fun_default;
      Public_data.Service_Labo_Dpt,fun_default;
      Public_data.Etablissement_ou_Entreprise,fun_default;
      Public_data.Credits,fun_default;
      Public_data.Type_de_Financement,fun_default;
      Public_data.Periode_de_Financement,fun_default;
      Public_data.Organisme_de_Financement,fun_default;
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
      store_diplome
        __POS__ ~who
        state
        current_file
        current_file'
        output
    else if
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
  | [a] -> state, Some a.gps_file
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

let lgen grade gps dpt d =
  if List.exists
      (fun diplome ->
         match diplome.grade with
         | None -> false
         | Some s ->
           simplify_string s = grade)
      d.diplomes
  then
    List.exists
      (fun diplome -> diplome.diplome_diplome=Some gps)
      d.diplomes
  else
    d.nannee = Some 1
    &&
    dpt <> ""
    &&
     (match d.departement_principal,d.departement_secondaire with
     | None, None -> false
     | Some x, None | None, Some x ->
       simplify_string x = dpt
     | Some x, Some y ->
       simplify_string x = dpt || simplify_string y = dpt)

let lmath d =
  lgen "licence" "gps2274" dpt_maths_gps_name d

let linfo d =
  lgen "licence" "gps2291" dpt_info_gps_name d

let lpoly d =
  lgen "licence" "gps74842" "" d

let lmathphys d =
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
         (diplome.diplome_diplome=Some "gps47622")
         || (diplome.diplome_diplome=Some "gps50382"))
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
      | Some _, None | None, Some _ ->
        false
      | Some x, Some y ->
        (simplify_string x = dpt_maths_gps_name && simplify_string y = dpt_phys_gps_name)
        || (simplify_string x = dpt_phys_gps_name && simplify_string y = dpt_maths_gps_name)
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
       (
         (Tools.map_opt String.trim diplome.diplome_diplome)=Some diplome'
         &&
         (Tools.map_opt String.trim diplome.niveau) = Some "2")
       || diplome.diplome_diplome=Some gps)
    d.diplomes
  ||
  List.exists
    (fun cours -> cours.code_cours = Some stage)
    d.cours

let mpri = gen_master "M-MPRI" "gps62263" "INFO-M2-MPRI200-S2"
let mva = gen_master "M-MVA" "gps2228" "INFO-M2-MVASTAGE-S2"
let iasd = gen_master "M-IASD" "gps76822" "INFO-M2-IASD-STG-S2"
let mash = gen_master "M-MASH" "gps59622" "INFO-M2-MASH-STG-S2"
let mint = gen_master "M-Interaction" "gps78864" "XT 00000000000647168"

let string_of_stringopt s_opt =
  match s_opt with
  | None -> ""
  | Some s -> s

let translate_dpt state d =
  match d with
  | None ->
    Remanent_state.warn_dft
      __POS__
      "Departement non rempli"
      Exit
      ""
      state
  | Some s ->
    begin
      match simplify_string s with
      | x when x=dpt_info  -> state, dpt_info_full
      | x when x=dpt_maths -> state, dpt_maths_full
      | x when x=dpt_phys  -> state, dpt_phys_full
      | x -> state,
             Printf.sprintf
               "Departement de %s"
               (Special_char.capitalize
                  (Special_char.lowercase x
                  ))
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
          let state, note_string =
            Notes.to_string __POS__ state note
          in
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


let keep_class
    ~firstname ~lastname ~year ~codecours ~note
    state filter =
  let state, compensation =
      get_compensation
        ~firstname ~lastname ~year ~codecours
        note state
  in
  let state, valide = keep_class state filter year note in
  state, valide || compensation

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

let is_dma_course code_cours year =
  Tools.substring "DMA" code_cours
  ||
  begin
    try
      let i = int_of_string year in
      if i <= 2015 then code_cours = "INFO-L3-MIIMC-S2"
      else if i <= 2018 then code_cours = "INFO-L3-THEOIC-S2"
      else code_cours = "INFO-L3-APPREN-S2"
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
        let state,label =
          match level with
          | "L" -> state, "L3 "^gerund
          | "M" -> state, "M1 "^gerund
          | _ ->
            let msg =
              Format.sprintf
                "Unknown class level (%s)"
                level
            in
            Remanent_state.warn_dft
              __POS__
              msg
              Exit
              ""
              state
        in
        let dpt =
          match dpt.Public_data.dpt_acronyme with
          | "DI" -> "informatique"
          | "DMA" -> "mathématiques"
          | "PHYS" -> "physique"
          | _ -> ""
        in
        state, (Some level,
                label,
                dpt)
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
        (None, "","")
        state
    end
  | state, None  ->
  let check_dpt pos state diplome label code_cours year situation =
    match situation.departement_principal with
    | None ->
      Remanent_state.warn_dft
        pos
        "Main teaching dpt is missing"
        Exit
        (Some diplome,label,"")
        state
    | Some dpt ->
      let dpt = Special_char.lowercase dpt in
      let dpt =
        if dpt = "mathématiques et applications"
        then
          "mathématiques"
        else
          dpt
      in
      if label = "L3" || label ="M1"
      then
        let state, (dpt, diplome)  =
          if dpt = "mathématiques"
          && not (is_dma_course code_cours year)
          then
            if is_di_course code_cours year
            then state, ("informatique", "L")
            else
            if is_phys_course code_cours year
            then state, ("physique","L")
            else
              let msg =
                Format.sprintf
                  "Cannot classify course %s (dpt:%s) for %s %s (%s)" code_cours dpt firstname lastname year
              in
              Remanent_state.warn_dft
                pos
                msg
                Exit
                (dpt,label)
                state
          else
            state, (dpt, diplome)
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
        state, (Some diplome,label,dpt)
      else
        state, (Some diplome,label,dpt)
  in
  match diplome with
  | Some "L" ->
    begin
      if lpoly situation
      then
        check_dpt __POS__ state "L"
          "Bachelor de l'École Polytechnique"
          code_cours year
          situation
      else
      if lmathphys situation
      then
        if is_dma_course code_cours year
        then
          state,
          (Some "L","L3 de mathématiques",dpt_maths)
        else
          state,
          (Some "L","L3 de physique",dpt_phys)
      else
      if linfo situation && lmath situation
      then
        if is_dma_course code_cours year
        then
          state,
          (Some "L","L3 de mathématiques",dpt_maths)
        else
          state,
          (Some "L","L3 d'informatique",dpt_info)
      else
      check_dpt __POS__ state
          "L" "L3" code_cours year
          situation
    end
  | Some "M" ->
    if mmaths situation then
      state, (Some "M","M1 de mathématiques",dpt_maths)
    else
    if mpri situation then
      state, (Some "MPRI","M2 du MPRI",dpt_info)
    else if mva situation then
      state, (Some "MVA","M2 du MVA",dpt_info)
    else if iasd situation then
      state, (Some "IASD","M2 IASD",dpt_info)
    else if mash situation then
      state, (Some "MASH","M2 MASH", dpt_info)
    else if mint situation then
      state, (Some "Interaction", "M2 Interaction", dpt_info)
    else
      check_dpt __POS__ state
        "M" "M1" code_cours year
        situation
  | Some ("DENS" | "dens") ->
    state, (Some ("DENS"), "DENS", "DENS")
  | Some x ->
    check_dpt __POS__ state
      x x code_cours year
      situation
  | None ->
    let state, (_,b,c) =
      check_dpt __POS__ state "" "" code_cours year situation
    in
    state, (None,b,c)



let color_of_dpt who pos state dpt =
  let dpt = simplify_string dpt in
  if dpt = dpt_info
  then state, Some Color.yellow
  else if dpt = dpt_maths
  then state, Some Color.orange
  else if dpt = dpt_phys
  then state, Some Color.duckblue
  else
    let msg =
      Format.sprintf "Unknown departement (%s) for %s"
        dpt who
    in
    Remanent_state.warn_dft
      pos
      msg
      Exit
      None
      state

let acro_of_dpt who pos state dpt =
  let dpt = simplify_string dpt in
  if dpt = dpt_info
  then state, Some acro_dpt_info
  else if dpt = dpt_maths
  then state, Some acro_dpt_maths
  else if dpt = dpt_phys
  then state, Some acro_dpt_phys
  else
    let msg =
      Format.sprintf "Unknown departement (%s) for %s"
        dpt who
    in
    Remanent_state.warn_dft
      pos
      msg
      Exit
      None
      state


let stage = 25
let memoire = 24
let ecla = -1
let dma = 0
let info = 1
let phys = 2
let autre = 3
let manquant = 4

let code_list =
  [
    stage, "STAGE";
    memoire, "memoire";
    memoire, "expose";
    ecla, "ECLA";
    dma, "DMA";
    info, "INFO";
    phys, "PHYS";
  ]

let fetch a =
  match (snd a).code_cours with
  | None -> manquant
  | Some code ->
  let rec aux l =
    match l with
    | [] -> autre
    | (a,b)::t ->
      if Tools.substring b code
      then a
      else aux t
  in
  aux code_list

let p (t,(_,cours)) (t',(_,cours')) =
  let cmp = compare t t' in
  if cmp = 0
  then compare cours.cours_libelle cours'.cours_libelle
  else cmp

let is_mandatory state cours =
  state,
  if match cours.code_cours with
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
    (fun x -> Format.sprintf "\\mandatory{%s}" x)
  else
    (fun x -> x)

let get_bourse ~firstname ~lastname ~er state =
  match Remanent_state.get_scholarship
          ~firstname ~lastname
          state
  with
  | state, None ->
    state, ""
  | state, Some scholarship ->
    state,
    Format.sprintf "Boursi%s %s" er
      scholarship.Public_data.organism

let export_transcript
    ~output ?filter:(remove_non_valided_classes=Public_data.All_but_in_progress_in_current_academic_year)
    state gps_file =
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
    with _ ->
      let () =
        Format.printf
          "Cannot open file %s@."
          file
      in
      Remanent_state.warn
        __POS__
        (Format.sprintf "Cannot open file %s"  file)
        Exit
        state ,
      None
  in
  match output_channel_opt with
  | None -> state, None
  | Some out ->
    let mode = Loggers.Latex Loggers.Lanscape in
    let logger = Loggers.open_logger_from_channel ~mode out in
    let old_logger = Remanent_state.save_std_logger state in
    let state = Remanent_state.set_std_logger state logger in
    let l = Public_data.YearMap.bindings gps_file.situation in
    let genre,er,ne =
      match gps_file.genre with
      | None -> "(e)","er(\\`ere)","(ne)"
      | Some Public_data.Masculin -> "","er",""
      | Some Public_data.Feminin -> "e","\\`ere","ne"
    in
    let lastname =
      Special_char.uppercase (Tools.unsome_string gps_file.nom)
    in
    let firstname =
      Special_char.capitalize (Tools.unsome_string gps_file.prenom)
    in
    let promo =
      (Tools.unsome_string gps_file.promotion)
    in
    let who =
      Format.sprintf
        "pour %s %s (%s)"
        firstname lastname promo
    in

    let l_rev,_ =
      List.fold_left
        (fun (l,counter) (y,annee) ->
           if
             match annee.situation_administrative
             with None ->
               (try int_of_string y<=2015 with _ -> false)
                | Some sit ->
                  simplify_string sit = "scolarite a l'ens"
           then
             let counter = counter + 1 in
             let nannee = Some counter in
             let annee = {annee with nannee} in
             ((y,annee)::l,counter)
           else
             (y,annee)::l,counter)

        ([],0) l
    in
    let year = promo in
    let state, picture_list =
      Remanent_state.get_picture_potential_locations
        ~firstname ~lastname ~year state
    in
    let state, b =
      let rec aux state l =
        match l with
        | [] -> state, false
        | h::t ->
          let state, b = Safe_sys.file_exists __POS__ state h
          in
          if b then state, true
          else aux state t
      in aux state picture_list
    in
    let state =
      if b
      then
        state
      else
        let msg =
          Printf.sprintf
            "No pictures for %s" who
        in
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
    in
    let state =
      List.fold_left
        (fun state (year,situation) ->
           let backgroundcolor = Some Color.green in
           let who =
             Format.sprintf "%s in %s" who year
           in
           let () =
             Remanent_state.log
               ?backgroundcolor
               state
               "D\\'epartement d'Informatique. \\'Ecole Normale  Sup\\'erieure. 45, rue d'Ulm 75005 Paris. Tel : +33 (0)1 44 32 20 45."
           in
           let () =
             Remanent_state.print_newline state in
           let backgroundcolor = Some Color.blue in
           let lineproportion = Some (2./.3.) in
           let state,statut,bourse =
            match gps_file.statut with
              | None -> state,"",""
              | Some Public_data.Boursier_si ->
                state,
                Format.sprintf "\\'Etudiant%s SI" genre,""
              | Some Public_data.Eleve_bis
              | Some Public_data.Eleve -> state,"\\'El\\`eve",""
              | Some Public_data.Ex_eleve ->
                state,
                Format.sprintf "Ancien%s \\'el\\`eve" ne,""
              | Some Public_data.Ex_etudiant ->
                begin
                  let state, bourse =
                    get_bourse ~firstname ~lastname ~er state
                  in
                  state,
                  Format.sprintf "Ancien%s \\'etudiant%s" ne genre,bourse
                end
              | Some Public_data.Etudiant ->
                begin
                  let state, bourse =
                    get_bourse ~firstname ~lastname ~er state
                  in
                  state,
                  Format.sprintf "\\'Etudiant%s" genre,bourse
                end
           in
           let () =
             Remanent_state.log
               ?backgroundcolor
               ?lineproportion
               state
               "\\large %s %s \\hspace{5mm} n\\'e%s le %s \\hspace{5mm} %s %s"
               (Special_char.uppercase (Tools.unsome_string
                                          gps_file.nom))
               (Special_char.capitalize (Tools.unsome_string gps_file.prenom))
               genre
               (Tools.unsome_string gps_file.date_de_naissance)
               statut
               bourse
        in
        let lineproportion = Some (1./.3.) in
        let () =
          Remanent_state.log
            ?backgroundcolor
            ?lineproportion
            state
            "\\large Promotion : %s"
            (Tools.unsome_string gps_file.promotion)
        in
        let () =
          Remanent_state.print_newline state
        in
        let state, tuteur =
            Remanent_state.get_mentoring
              ~year
              ~lastname
              ~firstname
              __POS__
              state
          in
        let state, (tuteur, lineproportion) =
          match tuteur with
          | None ->
            let msg =
              Printf.sprintf
                "Tuteur inconnu pour %s"
                who
            in
            Remanent_state.warn_dft
              __POS__
              msg
              Exit
              ("",1.)
              state
          | Some tuteur ->
            begin
              match
                tuteur.Public_data.nom_du_tuteur, tuteur.Public_data.prenom_du_tuteur,
                tuteur.Public_data.genre_du_tuteur,
                tuteur.Public_data.courriel_du_tuteur
              with
              | None, (None | Some _), (None | Some _), None ->
                let msg =
                  Printf.sprintf
                    "Tuteur inconnu pour %s"
                    who
                in
                Remanent_state.warn_dft
                  __POS__
                  msg
                  Exit
                  ("",1.)
                  state
              | Some x, Some y, Some z, _ ->
                state,
                (Printf.sprintf
                   "%s %s %s"
                   (match z with Public_data.Masculin -> "Tuteur : " | Public_data.Feminin -> "Tutrice : ")
                  (Special_char.capitalize y)
                  (Special_char.uppercase x), 2./.3.)
              | None, _, _, Some x -> state, (x, 2./.3.)
              | Some x, _, _, _ -> state, (x, 2./.3.)
            end
        in
        let backgroundcolor =
          match
            situation.nannee
          with
          | None -> Color.orange
          | Some _ -> Color.yellow
        in
        let textcolor = Color.red in
        let annee_int = int_of_string year in
        let annee =
          Printf.sprintf "%i -- %i" annee_int (annee_int+1)
        in
        let state, statut =
          match
            situation.nannee
          with
          | None -> state, "Cesure"
          | Some i ->
            begin
              let state, prefix =
                match i with
                | 1 -> state, "Première année :"
                | 2 -> state, "Seconde année :"
                | 3 -> state, "Troisème année :"
                | 4 -> state, "Quatrième année :"
                | _ ->
                let msg =
                  Printf.sprintf
                    "max 4 ans de scolarité pour %s"
                    who
                in
                  Remanent_state.warn_dft
                    __POS__
                    msg
                    Exit
                    ((string_of_int i)^"ème année :")
                    state
              in
              let state, suffix =
                if
                  lmath situation
                  &&
                  linfo situation
                then
                  let state, dpt =
                    match situation.departement_principal with
                    | Some x ->
                      let s  = simplify_string x in
                      if s = dpt_info_gps_name
                      then
                        state,acro_dpt_info
                      else if s = dpt_maths_gps_name
                      then state, acro_dpt_maths
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
                  state, Printf.sprintf
                    "Cursus maths-info et rattaché%s au %s" genre dpt
                else if
                  lmathphys situation
                then
                let state, dpt =
                  match situation.departement_principal with
                  | Some x ->
                    let s  = simplify_string x in
                    if s = dpt_phys_gps_name
                    then
                      state,acro_dpt_phys
                    else if s = dpt_maths_gps_name
                    then state, acro_dpt_maths
                    else
                    if s = dpt_info_gps_name
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
                state, Printf.sprintf
                  "Cursus maths-physique et rattaché au %s" dpt
                else
                  translate_dpt state situation.departement_principal
              in
              state, Printf.sprintf "%s %s" prefix suffix
            end
        in
        let () =
          Remanent_state.log
            ~lineproportion
            ~backgroundcolor
            ~textcolor
            state
            "%s %s "
            annee
            statut
        in
        let () =
          if not (tuteur = "") then
            let lineproportion = 1./.3. in
              Remanent_state.log
                ~lineproportion
                ~backgroundcolor
                ~textcolor
                state
                "%s"
                tuteur
        in
        let () =
          Remanent_state.print_newline state
        in
        let k =
          List.fold_left
            (fun n x ->
               let () =
                 Remanent_state.fprintf
                   state
                   "\\IfFileExists{%s}%%\n\ {\\vspace*{-2cm}{\\hfill\\includegraphics[height=2cm]{%s}}\\hspace*{5mm}\\mbox{}}%%\n\ {" x x
               in
               n+1)
            0
            picture_list
        in
        let () =
          Remanent_state.fprintf
            state
            "%s%%\n\ "
            (String.init k (fun _ -> '}'))
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
               match dispense.Public_data.dispense_motif with
               | None ->
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
               | Some motif ->
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
            ) state list
        in
        let () =
          Remanent_state.print_newline state
        in
        let state, filtered_classes =
          filter_class ~firstname ~lastname ~year
            state remove_non_valided_classes situation.cours
        in
        let state, split_cours =
          List.fold_left
            (fun (state,map) elt ->
               match elt.code_cours with
               | Some code_cours  ->
                 let state,
                     (diplome_key,diplome_label,diplome_dpt) =
                 translate_diplome
                   ~situation ~firstname ~lastname ~year ~code_cours state
                   elt.diplome
                 in
               state,
               addmap
                 (diplome_key,diplome_dpt)
                 (diplome_label,elt) map
               | None ->
                 Remanent_state.warn_dft
                   __POS__
                   "The code of a course is missing"
                   Exit
                   map
                   state
            )
            (state, StringOptMap.empty)
            filtered_classes
        in
        let l = [21.0;11.67;48.33;26.67;7.3;10.00;5.17] in
        let sum =
          List.fold_left
            (fun total a -> total+.a)
            0.
            l
        in
        let size =
          List.rev_map
            (fun a -> Some (a/.(sum*.1.12)))
            (List.rev l)
        in
        let () =
          Remanent_state.fprintf state "\n\ \\vfill\n\ \n\ "
        in
        let state =
          StringOptMap.fold
            (fun (string,dpt) list state ->
               let state, key, b = alloc_suffix (string,dpt) state in
               let () =
                 if b
                 then
                   let () =
                     Remanent_state.fprintf
                       state
                       "\\newcounter{validatedwogradeects%s}%%\n\ \\newcounter{grade%s}%%\n\ \\newcounter{gradedects%s}%%\n\ \\newcounter{potentialects%s}%%\n"
                       key key key key
                   in
                   ()
               in
               let () =
                 if b
                 then
                   let () =
                     Remanent_state.fprintf
                       state
                       "\\setcounter{validatedwogradeects%s}{0}%%\n\ \\setcounter{grade%s}{0}%%\n\ \\setcounter{gradedects%s}{0}%%\n\ \\setcounter{potentialects%s}{0}%%\n" key key key key
                   in
                   ()
               in
               let state, color =
                 match Tools.map_opt String.trim string
                 with
                 | None ->
                   let msg =
                     Printf.sprintf "Empty discipline category  for %s" who
                 in
                 Remanent_state.warn_dft
                   __POS__
                   msg
                   Exit
                   None
                   state
                 | Some ("DENS" | "dens") -> state, Some Color.blue
                 | Some ("LInfo" | "linfo") -> state, Some Color.yellow
                 | Some ("lmath" | "mmath" | "LMath" | "MMath") -> state, Some Color.orange
                 | Some ("m" | "l" | "m1" | "l3" | "M" | "L" | "M1" | "L3" | "mva" | "mpri" | "iasd" | "mash" | "interaction" ) ->
                   color_of_dpt who __POS__ state dpt
                 | Some x  ->
                   let msg =
                     Printf.sprintf "Unknown course category '%s' for %s" x who
                   in
                   Remanent_state.warn_dft
                     __POS__
                     msg
                     Exit
                     None
                     state

               in
               let bgcolor=[None;color;None;None;None;None;None] in

               let () =
                 Remanent_state.fprintf state
                   "\\setcounter{totalrows}{%i}%%%%\n\ "
                   (List.length list)
               in
               let state =
                 Remanent_state.open_array
                   __POS__
                   ~bgcolor
                   ~size
                   ~with_lines:true
                   ~title:["Code";"Dipl\\^ome";"Intitul\\'e"
                          ;"Enseignant";"Semestre";"Note";"ECTS"]
                   state
               in
               let macro = "cours" in
               let list = Tools.sort fetch p list in
               let state =
                 List.fold_left
                   (fun state (diplome,cours) ->
                      let () =
                        Remanent_state.open_row
                          ~macro state
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
                      let () =
                        Remanent_state.print_cell
                          codecours
                          state
                      in
                      let () =
                        Remanent_state.print_cell
                          diplome
                          state
                      in
                      let state, f =
                        is_mandatory state cours
                      in
                      let () =
                        Remanent_state.print_cell
                          (f
                             (string_of_stringopt
                                cours.cours_libelle))
                          state
                      in
                      let () =
                        Remanent_state.print_cell
                          (string_of_stringopt
                             cours.responsable)
                          state
                      in
                      let () =
                        Remanent_state.print_cell
                          (string_of_stringopt
                             cours.semestre)
                          state
                      in
                      let state, string =
                        match cours.note with
                        | None -> state, ""
                        | Some f ->
                          Notes.to_string __POS__ state f
                      in
                      let () =
                        Remanent_state.print_cell
                          string
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
                      state)
                   state
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
               let state, decision_opt =
                 match string with
                 | None -> state, None
                 | Some program ->
                   let state, dpt = acro_of_dpt who __POS__ state dpt in
                   let dpt =
                     match dpt with
                     | None -> ""
                     | Some a -> a
                   in
                   Remanent_state.get_decision
                     ~firstname
                     ~lastname
                     ~year
                     ~dpt
                     ~program
                     state
               in
               let moyenne_opt, mention_opt,
                   rank_opt, effectif_opt,
                   date_opt, commission_name_opt,
                   decision_opt, _validated_opt
                 =
                 match decision_opt with
                 | None -> None, None, None, None, None, None, None, None
                 | Some d ->
                   d.Public_data.decision_mean,
                   d.Public_data.decision_mention,
                   d.Public_data.decision_rank,
                   d.Public_data.decision_effectif,
                   d.Public_data.decision_date,
                   d.Public_data.decision_commission_name,
                   d.Public_data.decision_decision,
                   d.Public_data.decision_validated
               in
               let moyenne_value =
                 Format.sprintf
                   "\\thegrade%s/\\thegradedects%s"
                   key
                   key
               in
               let moyenne, update_moyenne, mention =
                 if string = Some "DENS" || string = Some "dens"
                 then "","",""
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
                       "\\fpeval{(\\thegradedects%s+\\thevalidatedwogradeects%s+\\thepotentialects%s)/\\factor} < 60"
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
                         Format.sprintf "Moyenne : \\numprint{\\fpeval{\\mean}}/20 \\hspace*{1cm}%%\n\ "]
                       ~otherwise:(Format.sprintf "Moyenne provisoire : \\numprint{\\fpeval{\\mean}}/20 \\hspace*{1cm}%%\n\ ")
                   in
                   let update_mean =
                     match moyenne_opt with
                     | None ->
                       Format.sprintf
                         "\\renewcommand{\\mean}{%s}" moyenne_value
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
                     if string = Some "DENS" || string = Some "dens"
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
                                Format.sprintf "\\fpeval{\\mean<12} = 1","";
                                Format.sprintf "\\fpeval{\\mean<14} = 1",
                                "Mention : Assez Bien";
                                Format.sprintf "\\fpeval{\\mean<16} = 1 ",
                                "Mention : Bien";
                              ]
                              ~otherwise:"Mention : Très Bien"]
                           ~otherwise:""
                       in
                       match mention_opt with
                     | None -> mention
                     | Some a ->
                       Format.sprintf
                         "Mention : %s \\hspace*{1cm}"
                         a
                   in
                   mean_string, update_mean, mention
               in
               let ects,pects =
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
                     "\\fpeval{(\\thegradedects%s+\\thevalidatedwogradeects%s)/\\factor}"
                     key key
                 in
                 let ects_string =
                   Latex_helper.case
                     Latex_helper.ifnum
                     [
                       Format.sprintf "%s=0" total_ects,"";
                       Format.sprintf "%s=%s" this_year_ects_amp total_ects_amp,
                       Format.sprintf "ECTS : %s " total_ects;
                     ]
                     ~otherwise:(Format.sprintf "ECTS (cumulés) : %s" total_ects)
                 in
                 let potential_ects_string  =
                   Latex_helper.ifnum
                     ~cond:(Format.sprintf "\\thepotentialects%s=0" key)
                     ~btrue:""
                     ~bfalse:(Format.sprintf "\\hspace*{0.2cm} (potentiellement  {{\\fpeval{(\\thegradedects%s+\\thevalidatedwogradeects%s+\\thepotentialects%s)/\\factor}}} ects)" key key key)
                 in
                 ects_string, potential_ects_string
               in
                let rank =
                 match rank_opt, effectif_opt with
                 | None, _ -> ""
                 | Some a, None ->
                   Format.sprintf "Rang : %i \\hspace*{1cm}" a
                 | Some a, Some b ->
                   Format.sprintf "Rang : %i/%i \\hspace*{1cm}" a b
               in
                let commission =
                 match
                   commission_name_opt, date_opt
                 with
                 | None, _ -> ""
                 | Some a, None ->
                   Format.sprintf
                     "Décision de %s \n\n"
                     a
                 | Some a, Some b ->
                   Format.sprintf
                     "Décision de %s du %s \n\n"
                     a b
               in
               let decision =
                 match decision_opt with
                 | None -> ""
                 | Some x ->
                   Format.sprintf "%s \\hspace*{1cm}" x
               in
               let msg =
                 Format.sprintf
                   "%s\\nprounddigits{2}%%\n\ %s%s%s%s\\npnoround%%\n\ \n\n%s%s%s"
                   commission
                   update_moyenne moyenne
                   ects pects
                   decision rank mention
               in
               let () =
                 Remanent_state.print_cell msg state
               in
               let () =
                 Remanent_state.fprintf state "\\vfill\n\ "
               in
               let () =
                 Remanent_state.print_newline state
               in
               let () =
                 Remanent_state.print_newline state
               in
               let () =
                 Remanent_state.print_newline state
               in
               state)
            split_cours
            state
        in
        let () =
          Remanent_state.breakpage state
        in
        state
        )
        state
        (List.rev l_rev)
    in
    let state = Remanent_state.close_logger state in
    let state =
      Remanent_state.restore_std_logger state old_logger
    in
    state, Some (rep, snd output)
