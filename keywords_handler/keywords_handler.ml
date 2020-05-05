module KeyWordsMap =
  Map.Make
    (struct
      type t = Public_data.keywords
      let compare = compare
    end)

let asso_list =
  [
    Public_data.LastName, ["nom"];
    Public_data.FirstName, ["prénom";];
    Public_data.Courriel, ["courriel"];
    Public_data.Statut, ["statut"];
    Public_data.Promo, ["promo"];
    Public_data.Origine, ["origine"];
    Public_data.Departement, ["département"];
    Public_data.Contrat, ["contrat"];
    Public_data.Recu, ["reçu"];
    Public_data.Pers_id, ["pers_id"];
    Public_data.Ignore, ["ignore"];
]


let asso_list =
  List.rev_map
    (fun (a,b) ->
       (a,
        List.flatten
          (List.rev_map Special_char.expand_string (List.rev b))))
    (List.rev asso_list)

let make state function_table =
  let event_opt =
    Some Profiling.Build_keywords_automaton
  in
  let state = Remanent_state.open_event_opt event_opt state
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
           Remanent_state.warn_dft
             __POS__
             ""
             Exit
             output
             state
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
  let action state x y =
    let state, asso = asso state x y in
    state,
    match asso with
    | None -> None
    | Some asso -> Some (snd asso)
  in
  let translate state x y =
    let state, asso = asso state x y in
    state,
    match asso with
    | None -> None
    | Some asso -> Some (fst asso)
  in
  let state =
    Remanent_state.close_event_opt event_opt state
  in
  state, is_keyword, translate, action
