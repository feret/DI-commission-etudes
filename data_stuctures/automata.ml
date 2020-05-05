module IntMap =
  Map.Make
    (struct
      type t = int
      let compare = compare
    end)

module CharMap =
  Map.Make
    (struct
      type t = char
      let compare = compare
    end)

type 'a node =
  {
    data: 'a option;
    succ: int CharMap.t;
  }

let empty_node =
  {
    data = None;
    succ = CharMap.empty;
  }

type 'a intermediary =
  {
    mapping: 'a node IntMap.t;
    fresh_key: int
  }

let empty =
  {
    mapping = IntMap.add 0 empty_node IntMap.empty;
    fresh_key = 1
  }


let get_succ remanent_state node_idx c automaton =
  match
    IntMap.find_opt node_idx automaton.mapping
  with
  | None ->
    Remanent_state.warn_dft
      __POS__
      ""
      Exit
      (node_idx,automaton)
      remanent_state
  | Some node ->
    begin
      match
        CharMap.find_opt c node.succ
      with
      | Some idx ->
        remanent_state, (idx, automaton)
      | None ->
        let idx = automaton.fresh_key in
        let succ = CharMap.add c idx node.succ in
        let node = {node with succ} in
        let mapping = IntMap.add node_idx node automaton.mapping in
        let mapping = IntMap.add idx empty_node mapping in
        let fresh_key = automaton.fresh_key + 1 in
        remanent_state, (idx, {mapping; fresh_key})
    end

let store_association remanent_state node_idx data automaton =
  match
    IntMap.find_opt node_idx automaton.mapping
  with
  | None ->
    Remanent_state.warn_dft
      __POS__
      ""
      Exit
      automaton
      remanent_state
  | Some node ->
    begin
      match node.data with
      | Some _ ->
      Remanent_state.warn_dft
        __POS__
        ""
        Exit
        automaton
        remanent_state
      | None ->
        let node = {node with data} in
        let mapping = IntMap.add node_idx node automaton.mapping in
        remanent_state, {automaton with mapping}
    end

let find_node_idx remanent_state key automaton =
  let n = String.length key in
  let rec visit remanent_state node k automaton  =
    if k=n then remanent_state, node, automaton
    else
      let remanent_state, (node, automaton) =
        get_succ remanent_state node (String.get key k) automaton
      in
      visit remanent_state node (k+1) automaton
  in
  visit remanent_state 0 0 automaton

let add_association remanent_state key data automaton =
  let remanent_state, node, automaton =
    find_node_idx remanent_state key automaton
  in
  store_association remanent_state node data automaton

let stabilize automaton =
  let n = automaton.fresh_key in
  let array = Array.make n empty_node in
  let _ =
    IntMap.iter
      (fun i node -> array.(i) <- node)
      automaton.mapping
  in
  array

let follow_succ_stab pos remanent_state node_idx c automaton =
  try
    let node = automaton.(node_idx) in
    match
      CharMap.find_opt c node.succ
    with
    | Some idx ->
      remanent_state, Some idx
    | None ->
      remanent_state, None
  with
  | _ ->
    Remanent_state.warn_dft
      pos
      ""
      Exit
      None
      remanent_state

let check_node_idx_stab pos remanent_state key automaton =
  let n = String.length key in
  let rec visit pos remanent_state node k automaton =
    if k=n then remanent_state,
                begin
                  match (automaton.(node)).data with
                  | Some _ -> true
                  | None -> false
                end
    else
      let remanent_state, node_opt =
        follow_succ_stab pos
          remanent_state node (String.get key k) automaton
      in
      match node_opt with
      | None -> remanent_state, false
      | Some node ->
      visit pos remanent_state node (k+1) automaton
  in
  visit pos remanent_state 0 0 automaton

let exists pos remanent_state key automaton =
    check_node_idx_stab pos remanent_state key automaton


let find_association pos remanent_state key automaton =
    try
      match (automaton.(key)).data with
      | None ->
        Remanent_state.warn_dft
          pos
          "No association"
          Exit
          None
          remanent_state
      | Some a ->
        remanent_state, Some a
    with
    | _ ->
      Remanent_state.warn_dft
        pos
        "Out of bounds access"
        Exit
        None
        remanent_state

let get_succ_stab pos remanent_state node_idx c automaton =
  try
    let node = automaton.(node_idx) in
    match
      CharMap.find_opt c node.succ
    with
    | Some idx ->
      remanent_state, idx
    | None ->
      Remanent_state.warn_dft
        pos
        ""
        Exit
        node_idx
        remanent_state
  with
  | _ ->
    Remanent_state.warn_dft
      pos
      ""
      Exit
      node_idx
      remanent_state

let find_node_idx_stab pos remanent_state key automaton =
  let n = String.length key in
  let rec visit pos remanent_state node k automaton =
    if k=n then remanent_state, node
    else
      let remanent_state, node =
        get_succ_stab pos
          remanent_state node (String.get key k) automaton
      in
      visit pos remanent_state node (k+1) automaton
  in
  let rep = visit pos remanent_state 0 0 automaton in
  rep

let find_association pos remanent_state key automaton =
  let remanent_state, node =
    find_node_idx_stab pos remanent_state key automaton
  in
  find_association pos remanent_state node automaton


let build remanent_state asso =
  let automaton = empty in
  let remanent_state, automaton =
    List.fold_left
      (fun (remanent_state, automaton) (s,data) ->
         add_association remanent_state s (Some data) automaton)
      (remanent_state, automaton)
      asso
  in
  let automaton = stabilize automaton in
  let test pos remanent_state key =
    exists pos remanent_state key automaton
  in
  let f pos remanent_state key =
    find_association pos remanent_state key automaton
  in
  remanent_state, test , f
