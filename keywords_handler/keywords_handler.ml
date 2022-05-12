module KeyWordsMap =
  Map.Make
    (struct
      type t = Public_data.keywords
      let compare = compare
    end)

type ('record_tmp) any_field_short =
  { key: Public_data.keywords;
    store:
      Remanent_state.t ->
      string option -> 'record_tmp -> Remanent_state.t * 'record_tmp}

type ('record_tmp,'record) any_field =
  { keyword: Public_data.keywords;
    set_tmp:
      Remanent_state.t ->
      string option -> 'record_tmp -> Remanent_state.t * 'record_tmp ;
    update:
      Remanent_state.t -> 'record_tmp -> 'record -> Remanent_state.t * 'record;
    is_unifyable: Remanent_state.t -> 'record -> 'record -> Remanent_state.t * bool ;
    unify: Remanent_state.t -> 'record -> 'record -> Remanent_state.t * 'record option;
    label_tmp: Remanent_state.t -> 'record_tmp -> Remanent_state.t * string option;
    label1:
      Remanent_state.t ->
      'record -> Remanent_state.t * string option;
    label2:
      Remanent_state.t ->
      'record -> 'record -> Remanent_state.t * string option
  }

let shorten a =
  {key = a.keyword; store = a.set_tmp}

type 'a shared =
  {
    do_at_end_of_file:
      Remanent_state.t -> 'a -> 'a list ->
      Remanent_state.t * 'a list;
    do_at_end_of_array_line:
      Public_data.keywords option list ->
      Remanent_state.t -> 'a -> 'a ->
      'a list ->
      Remanent_state.t * 'a * 'a list;
    do_at_end_of_array:
      Public_data.keywords option list ->
      Remanent_state.t ->
      'a ->
      'a list ->
      Remanent_state.t * 'a * 'a list;
    flush:
      Remanent_state.t -> 'a -> 'a list -> Remanent_state.t * 'a list;
  }

type 'record_tmp specification =
  {
    keywords: Public_data.keywords list;
    of_interest: Public_data.keywords list;
    all_fields:
      'record_tmp any_field_short list;
    default:
      (Remanent_state.t ->
       string option ->
       'record_tmp -> Remanent_state.t * 'record_tmp) ;
    shared_functions: 'record_tmp shared
  }

type 'a preprocessed =
  {
    is_keyword:
      string * int * int * int ->
      Remanent_state.t -> string -> Remanent_state.t * bool;
    action:
      string * int * int * int ->
      Remanent_state.t -> string ->
      Remanent_state.t *
      (Remanent_state.t ->
       string option ->
       'a -> Remanent_state.t * 'a)
        option;
    translate :
      string * int * int * int ->
      Remanent_state.t ->
      string -> Remanent_state.t * Public_data.keywords option;
    flush_required :
      string * int * int * int ->
      Remanent_state.t -> string -> Remanent_state.t * bool option;
    shared: 'a shared
  }

let asso_list =
  [
    Public_data.Accord, ["accord"];
    Public_data.Acronyme, ["acronyme"];
    Public_data.Annee_Academique, ["année"; "année académique"];
    Public_data.Annee_Debut, ["année début"];
    Public_data.Annee_Fin, ["année fin"];
    Public_data.Annee_en_Cours, ["année en cours"];
    Public_data.Classement, ["classement";"rang"];
    Public_data.Code, ["code";"code cours"];
    Public_data.Code_gps, ["code gps"];
    Public_data.Commentaire, ["commentaire"];
    Public_data.Commission, ["commission"];
    Public_data.Commission_en, ["commission_en";"commission(anglais)";"commission(english)"];
    Public_data.Contact_ENS, ["contact";"contact ENS"];
    Public_data.Contrat, ["contrat"];
    Public_data.Couleur_du_fond, ["couleur du fond"];
    Public_data.Couleur_du_texte, ["couleur du texte"];
    Public_data.Courriel, ["courriel"];
    Public_data.Courriel_du_tuteur, ["courriel du tuteur";"email du tuteur"];
    Public_data.Credits, ["crédits"];
    Public_data.Date, ["date"];
    Public_data.Date_en, ["date(english)";"date(anglais)";"date_en"];
    Public_data.Date_de_Naissance, ["date de naissance";"naissance"];
    Public_data.Decision, ["décision"];
    Public_data.Decision_en, ["décision_en";"decision(english)";"decision(anglais)"];
    Public_data.Departement, ["département"];
    Public_data.Departements,["département(s)"];
    Public_data.Departement_principal, ["principal";"département principal"];
    Public_data.Departement_secondaire, ["secondaire";"département secondaire"];
    Public_data.Derniere_Annee, ["derniere année"];
    Public_data.Diplome, ["diplôme"];
    Public_data.Diplomes, ["diplôme(s)"];
    Public_data.Directeur_de_Stage, ["directeur de stage"];
    Public_data.Directeur_Sujet, ["directeur -- sujet de recherche"];
    Public_data.Discipline_SISE,["discipline SISE"];
    Public_data.Duree,["durée"];
    Public_data.ECTS,["ECTS*";"ECTS"];
    Public_data.Effectif,["effectif"];
    Public_data.Enseignements,["enseignement(s)"];
    Public_data.Entete,["entete";"en-tete";"en_tete"];
    Public_data.Entete_en,[
                           "headpage";
                           "entete_en";"en-tete_en";"en_tete_en";
                           "entete(anglais)";"en-tete(anglais)";"en_tete(anglais)";
                           "entete(english)";"en-tete(english)";"en_tete(english)";
                          ];
    Public_data.Entree_GPS,["entrée GPS"];
    Public_data.Etablissement,["établissement"];
    Public_data.Etablissement_ou_Entreprise,["établissement ou entreprise"];
    Public_data.FirstName, ["prénom";];
    Public_data.FullName, ["nom complet"];
    Public_data.Genitif, ["genitif"];
    Public_data.Genre, ["genre"];
    Public_data.Genre_du_tuteur, ["genre du tuteur"];
    Public_data.Grade, ["grade"];
    Public_data.Inscription, ["inscription"];
    Public_data.Inscription_en, ["inscription_en";"registration";"inscription(anglais)";"inscription(english)"];
    Public_data.Inscrit_au_DENS_en, ["inscrit au DENS en"];
    Public_data.Intitule, ["intitulé"];
    Public_data.Label, ["label";"libellé(anglais)";"libellé_en"];
    Public_data.LastName, ["nom"];
    Public_data.Lettre, ["lettre"];
    Public_data.Libelle, ["libellé"];
    Public_data.Mention, ["mention"];
    Public_data.Motif, ["motif";"raison"];
    Public_data.Motif_en, ["reason";"raison_en";"motif_en";"motif(english)";"motif(anglais)"];
    Public_data.Moyenne, ["moyenne"];
    Public_data.Name, ["cours"];
    Public_data.Name_en, ["course";"cours_en";"cours(anglais)";"cours(english)"];
    Public_data.Niveau, ["niveau";"level"];
    Public_data.Nom_du_tuteur, ["nom du tuteur"];
    Public_data.Note, ["note"];
    Public_data.Obtenu_en, ["obtenu en"];
    Public_data.Option, ["option"];
    Public_data.Options, ["options"];
    Public_data.Origine, ["origine";"concours"];
    Public_data.Organisme_de_Financement, ["financement";"financeur";"organisme de financement"];
    Public_data.Periode, ["période"];
    Public_data.Periode_de_Financement, ["période de financement"];
    Public_data.Pers_id, ["pers_id"];
    Public_data.Pied_de_page, ["pied de page";"pied-de-page";"pied_de_page"];
    Public_data.Pied_de_page_en, ["footpage";
                                  "pied de page_en";"pied-de-page_en";"pied_de_page_en";
                                  "pied de page(english)";"pied-de-page(english)";"pied_de_page(english)";
                                  "pied de page(anglais)";"pied-de-page(anglais)";"pied_de_page(anglais)";
                                 ];

    Public_data.Pour_Diplome, ["pour diplôme"];
    Public_data.Prenom_du_tuteur, ["prénom du tuteur"];
    Public_data.Promo, ["promo";"promotion"];
    Public_data.Programme, ["programme"];
    Public_data.Programme_d_etude,
    ["programme";"programme d'études"; "Pgm études"];
    Public_data.Recu, ["reçu"];
    Public_data.Responsable, ["responsable"];
    Public_data.Responsable_local,
    ["responsable local"];
    Public_data.Secondaire, ["secondaire";"secondary"];
    Public_data.Semestre, ["semestre"];
    Public_data.Service_Labo_Dpt,
    ["service/labo/dpt"];
    Public_data.Situation, ["situation"];
    Public_data.Sujet_du_Stage_Type_du_Sejour,["sujet du stage / Type du séjour"];
    Public_data.Stages_et_Sejours_a_l_Etranger,
    ["Stage(s) & Séjour(s) à l'étranger"];
    Public_data.Statut, ["statut"];
    Public_data.Tuteur, ["tuteur"];
    Public_data.Type_de_Financement,["type de financement"];
    Public_data.Universite,["universite";"université";"university"];
    Public_data.Valide, ["validé"];
    Public_data.Ignore, ["ignore"];
]

(*let flatten l =
  let rec aux l output =
    match l with
    | [] -> output
    | []::tail -> aux tail output
    | (h::t)::tail ->
      aux (t::tail) (h::output)
  in
  aux l []
*)
(*let asso_list =
  List.rev_map
    (fun (a,b) ->
       (a,
        flatten
          (List.rev_map Special_char.expand_string (List.rev b))))
    (List.rev asso_list)*)

let asso_list =
  List.rev_map
    (fun (a,b) ->
       (a,
        List.rev_map Special_char.correct_string (List.rev b)))
    (List.rev asso_list)


let make state specification =
  let event_opt =
    Some Profiling.Build_keywords_automaton
  in
  let state = Remanent_state.open_event_opt event_opt state
  in
  let function_table =
    Tools.asso_list_map2
      specification.all_fields
      specification.keywords
      (fun x -> x.key)
      (fun x -> x)
      (fun x -> x.key, x.store)
      (fun x -> x,specification.default)
      (fun x _ -> x.key, x.store)
  in
  let function_table =
    Tools.asso_list_map2
      function_table
      specification.of_interest
      fst
      (fun x -> x)
      (fun (x,y) -> (x,(y,false)))
      (fun x -> (x,(specification.default,true)))
      (fun (x,y) _ -> (x,(y,true)))
  in
  let map =
    List.fold_left
      (fun map (a,b) -> KeyWordsMap.add a b map)
      KeyWordsMap.empty
      function_table
  in
  let state, asso_list =
    List.fold_left
      (fun (state, output) (a,list) ->
         match KeyWordsMap.find_opt a map with
         | None ->
           state, output
         | Some data ->
           state,
           List.fold_left
             (fun output elt ->
                (elt,(a,data))::output)
             output
             list)
      (state, [])
      (List.rev asso_list)
  in
  let state, is_keyword, asso =
    Automata.build state
      asso_list
  in
  let is_keyword a b c =
    let state, output = is_keyword a b (Special_char.correct_string c) in
    state, output
  in
  let cache = ref (None) in
  let asso pos state x =
    match !cache with
    | Some ((state',x'),output)
      when state'==state && x'==x
      ->
      output
    | _ ->
      let output = asso pos state x in
      let () = cache:=Some ((state,x),output) in
      output
  in
  let gen f state x y =
    let state, asso = asso state x (Special_char.correct_string y) in
    state,
    match asso with
    | None -> None
    | Some asso -> Some (f asso)
  in
  let action = gen (fun x -> fst (snd x)) in
  let action a b c =
    if c = "" then b,Some (fun a _ c -> a,c)
    else action a b c
  in
  let flush_required = gen (fun x -> snd (snd x)) in
  let translate = gen fst in
  let state =
    Remanent_state.close_event_opt event_opt state
  in
  let shared = specification.shared_functions in
  let is_keyword pos state x =
    is_keyword pos state (Special_char.correct_string x)
  in
  state,
  {
    is_keyword;
    translate;
    action;
    flush_required;
    shared;
  }
