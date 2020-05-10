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
    let state, asso = asso state x y in
    state,
    match asso with
    | None -> None
    | Some asso -> Some (f asso)
  in
  let action = gen (fun x -> fst (snd x)) in
  let flush_required = gen (fun x -> snd (snd x)) in
  let translate = gen fst in
  let state =
    Remanent_state.close_event_opt event_opt state
  in
  let shared = specification.shared_functions in
  state,
  {
    is_keyword;
    translate;
    action;
    flush_required;
    shared;
  }
