type annee = int

module ParAnnee =
  Map.Make (struct type t = annee let compare = compare end)

type diplome =
  {
    grade: string option;
    niveau: string option;
    diplome: string option;
    libelle: string option;
    etabliseement: string option;
    discipline_SISE: string option;
    obtenu_en: string option;
    directeur_sujet_de_recherche: string option;
  }

let empty_diplome =
  {
    grade = None;
    niveau = None;
    diplome = None;
    libelle = None;
    etabliseement = None;
    discipline_SISE = None;
    obtenu_en = None;
    directeur_sujet_de_recherche = None;

  }

type cours =
  {
    semestre: string option;
    code_cours: string option;
    responsable: string option;
    libelle: string option;
    etablissement: string option;
    duree: int option;
    ects: int option;
    diplome: string option;
    contrat: bool option;
    accord: bool option;
    note: int option;
    lettre: string option;
    commentaire: string list
  }

let empty_cours =
  {
    semestre = None ;
    code_cours = None ;
    responsable = None ;
    libelle = None ;
    etablissement = None ;
    duree = None ;
    ects = None ;
    diplome = None ;
    contrat = None ;
    accord = None ;
    note = None ;
    lettre = None ;
    commentaire = [];
  }
type specialisation =
  {
    code: string option;
    option: string option;

  }

let empty_specialisation =
  {
    code = None ;
    option = None ;
  }

type bilan_annuel =
       {
         annee: int option;
         situation_administrative: string option;
         programme_d_etudes: string option;
         derniere_annee: bool option;
         departement_principal: string option;
         departement_secondaire: string option;
         options: specialisation list;
         diplomes: diplome list;
         inscription_au_DENS: bool option;
         cours: cours list;
       }

let empty_bilan_annuel =
  {
    annee = None ;
    situation_administrative = None ;
    programme_d_etudes = None ;
    derniere_annee = None ;
    departement_principal = None ;
    departement_secondaire = None ; options = [] ;
    diplomes = [] ;
    inscription_au_DENS = None;
    cours = []}

type gps_file =
  {
    nom: string option;
    prenom: string option;
    nom_complet: string option;
    date_de_naissance: string option;
    promotion: string option;
    origine: string option;
    statut: string option;
    annee_en_cours: int option;
    contact_ens: string option;
    tuteur: string option;
    situation: bilan_annuel ParAnnee.t
  }


let empty_gps =
  {
    nom = None ;
    prenom = None ;
    nom_complet = None ;
    date_de_naissance = None ;
    promotion = None ;
    origine = None ;
    statut = None ;
    annee_en_cours = None ;
    contact_ens = None ;
    tuteur = None ;
    situation = ParAnnee.empty ;
  }

type remanent =
  {gps_file: gps_file;
   cours: cours;
   diplome: diplome;
   option: specialisation;
   inscription_DENS: bool option;
   sit_adm: string option;
   prg_et: string option;
   annee_de_travail : int option;
   last_year: bool option;
   dpt_principal: string option;
   dpt_secondaire: string option;
  }

let get_bilan_annuel state remanent year =
  state,
  match
    ParAnnee.find_opt
      year
      remanent.gps_file.situation
  with
  | Some a -> a
  | None ->
    empty_bilan_annuel

let set_bilan_annel state remanent year bilan =
  let situation =
    ParAnnee.add
      year
      bilan
      remanent.gps_file.situation
  in
  let gps_file =
    {remanent.gps_file with situation}
  in
  state, {remanent with gps_file}

let store_diplome state remanent =
  match remanent.annee_de_travail
  with
  | None ->
  Remanent_state.warn_dft
    __POS__
    "L'année manque"
    Exit
    remanent
    state
  | Some year ->
    let state, bilan = get_bilan_annuel
        state
        remanent
        year
    in
    let diplomes =
      remanent.diplome::bilan.diplomes
    in
    let bilan =
      {bilan with diplomes}
    in
    set_bilan_annel
      state
      remanent
      year
      bilan

let empty_remanent =
  {
    gps_file = empty_gps ;
    cours = empty_cours ;
    option= empty_specialisation ;
    diplome = empty_diplome ;
    inscription_DENS = None ;
    sit_adm = None ;
    prg_et = None ;
    last_year = None ;
    dpt_principal = None ;
    dpt_secondaire = None ;
    annee_de_travail = None ;
  }


let fun_default =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.LastName;
    Public_data.FirstName;
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
    Public_data.Code;
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
  Public_data.Code;
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

let asso_list =
    [
      Public_data.LastName,
      (fun state nom x ->
         let gps_file =
           {x.gps_file with nom}
         in
         state,{x with gps_file});

      Public_data.FirstName,
      (fun state prenom x ->
         let gps_file =
           {x.gps_file with prenom}
         in
         state,{x with gps_file});
      Public_data.Date_de_Naissance,fun_default;
      Public_data.Promo,fun_default;
      Public_data.Origine,fun_default;
      Public_data.Statut,fun_default;
      Public_data.Annee_en_Cours,fun_default;
      Public_data.Contact_ENS,fun_default;
      Public_data.Tuteur,fun_default;
      Public_data.Situation,fun_default;
      Public_data.Annee_Academique,fun_default;
      Public_data.Programme_d_etude,fun_default;
      Public_data.Derniere_Annee,fun_default;
      Public_data.Departements,fun_default;
      Public_data.Departement_principal,fun_default;
      Public_data.Departement_secondaire,fun_default;
      Public_data.Options,fun_default;
      Public_data.Code,fun_default;
      Public_data.Option,fun_default;
      Public_data.Diplomes,fun_default;
      Public_data.Grade,fun_default;
      Public_data.Niveau,fun_default;
      Public_data.Diplome,fun_default;
      Public_data.Libelle,fun_default;
      Public_data.Etablissement,fun_default;
      Public_data.Discipline_SISE,fun_default;
      Public_data.Obtenu_en,fun_default;
      Public_data.Directeur_Sujet,fun_default;
      Public_data.Enseignements,fun_default;
      Public_data.Inscrit_au_DENS_en,fun_default;
      Public_data.Semestre,fun_default;
      Public_data.Code,fun_default;
      Public_data.Responsable,fun_default;
      Public_data.Duree,fun_default;
      Public_data.ECTS,fun_default;
      Public_data.Pour_Diplome,fun_default;
      Public_data.Contrat,fun_default;
      Public_data.Accord,fun_default;
      Public_data.Valide,fun_default;
      Public_data.Note,fun_default;
      Public_data.Lettre,fun_default;
      Public_data.Commentaire,fun_default;
      Public_data.Stages_et_Sejours_a_l_Etranger,fun_default;
      Public_data.Periode,fun_default;
      Public_data.Sujet_du_Stage_Type_du_Sejour,fun_default;
      Public_data.Directeur_de_Stage,fun_default;
      Public_data.Responsable_local,fun_default;
      Public_data.Service_Labo_Dpt,fun_default;
      Public_data.Etablissement_ou_Entreprise,fun_default;
      Public_data.Credits,fun_default;
      Public_data.Valide,fun_default;
      Public_data.Accord,fun_default;
      Public_data.Type_de_Financement,fun_default;
      Public_data.Periode_de_Financement,fun_default;
      Public_data.Organisme_de_Financement,fun_default;
    ]

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
  let event_opt =
    Some
      (Profiling.Export_transcript
         (Some file_name))
  in
  let at_end_of_array_line
      header state current_file current_file' output =
    if List.mem
        (Some Public_data.Situation)
        header
    then state, current_file, output
    else if
      List.mem
        (Some Public_data.Departement_principal)
         header
    then state, current_file, output
    else if
      List.mem
        (Some Public_data.Option) header
      || List.mem
        (Some Public_data.Code)
        header
    then state, current_file, output
    else if
      List.mem
        (Some Public_data.Diplome) header
    then
      let state, current_file' =
        store_diplome
          state current_file'
      in
      let gps_file =
        current_file'.gps_file
      in
      state,
      {current_file with
       gps_file}
      , output
    else if
      List.mem
        (Some Public_data.Note)
        header
    then state, current_file, output
    else if
      List.mem
        (Some Public_data.Periode)
        header
    then state, current_file, output
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
      Keywords_handler.asso = asso_list ;
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
    Remanent_state.warn_dft
      __POS__
      "ill-formed output"
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

let export_transcript ~output state gps_file =
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
          "Cannot open file %s@ "
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
    let logger = Loggers.open_logger_from_channel out in
    let () =
      match gps_file.nom
      with
      | None -> ()
      | Some nom ->
        Loggers.fprintf
        logger
        "%s" nom
    in
    let () =
      Loggers.print_newline logger
    in
    let () = close_out out in
    state, Some (rep, snd output)
