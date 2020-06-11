module KeyWordsMap =
  Map.Make
    (struct
      type t = Public_data.keywords
      let compare = compare
    end)

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

type 'a specification =
  {
    keywords: Public_data.keywords list;
    of_interest: Public_data.keywords list;
    asso:
      (Public_data.keywords *
       (Remanent_state.t ->
        string option ->
        'a -> Remanent_state.t * 'a)) list;
    default:
      (Remanent_state.t ->
       string option ->
       'a -> Remanent_state.t * 'a) ;
    shared_functions: 'a shared
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
    Public_data.Annee_Academique, ["année"; "année académique"];
    Public_data.Annee_en_Cours, ["année en cours"];
    Public_data.Code, ["code";"code cours"];
    Public_data.Commentaire, ["commentaire"];
    Public_data.Contact_ENS, ["contact";"contact ENS"];
    Public_data.Contrat, ["contrat"];
    Public_data.Courriel, ["courriel"];
    Public_data.Courriel_du_tuteur, ["courriel du tuteur"];
    Public_data.Credits, ["crédits"];
    Public_data.Date_de_Naissance, ["Date de naissance";"naissance"];
    Public_data.Departement, ["département"];
    Public_data.Departement_principal, ["principal"];
    Public_data.Departements,["département(s)"];
    Public_data.Departement_secondaire, ["secondaire"];
    Public_data.Derniere_Annee, ["derniere année"];
    Public_data.Diplome, ["diplôme"];
    Public_data.Diplomes, ["diplôme(s)"];
    Public_data.Directeur_de_Stage, ["directeur de stage"];
    Public_data.Directeur_Sujet, ["directeur -- sujet de recherche"];
    Public_data.Discipline_SISE,["discipline SISE"];
    Public_data.Duree,["durée"];
    Public_data.ECTS,["ECTS*"];
    Public_data.Enseignements,["enseignement(s)"];
    Public_data.Etablissement,["établissement"];
    Public_data.Etablissement_ou_Entreprise,["établissement ou entreprise"];
    Public_data.FirstName, ["prénom";];
    Public_data.FullName, ["nom complet"];
    Public_data.Genre, ["genre"];
    Public_data.Grade, ["grade"];
    Public_data.Inscrit_au_DENS_en, ["inscrit au DENS en"];
    Public_data.LastName, ["nom"];
    Public_data.Lettre, ["lettre"];
    Public_data.Libelle, ["libellé"];
    Public_data.Niveau, ["niveau"];
    Public_data.Nom_du_tuteur, ["nom du tuteur"];
    Public_data.Note, ["note"];
    Public_data.Obtenu_en, ["obtenu en"];
    Public_data.Option, ["option"];
    Public_data.Options, ["options"];
    Public_data.Origine, ["origine";"concours"];
    Public_data.Organisme_de_Financement, ["financement";"financeur";"organisme de financement"];
    Public_data.Periode, ["période"];
    Public_data.Periode_de_Financement, ["période de finanement"];
    Public_data.Pers_id, ["pers_id"];
    Public_data.Pour_Diplome, ["pour diplôme"];
    Public_data.Prenom_du_tuteur, ["prénom du tuteur"];
    Public_data.Promo, ["promo";"promotion"];
    Public_data.Programme_d_etude,
    ["programme";"programme d'études"; "Pgm études"];
    Public_data.Recu, ["reçu"];
    Public_data.Responsable, ["responsable"];
    Public_data.Responsable_local,
    ["responsable local"];
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
      specification.asso
      specification.keywords
      fst
      (fun x -> x)
      (fun x -> x)
      (fun x -> x,specification.default)
      (fun x _ -> x)
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