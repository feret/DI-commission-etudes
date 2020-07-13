type t = Public_data.dpt Public_data.AcronymMap.t

let empty = Public_data.AcronymMap.empty

let get_dpt  ~acronym dpt =
  let acronym =
    String.lowercase_ascii acronym
  in
    Public_data.AcronymMap.find_opt
      acronym
      dpt

let add_dpt
    unify pos state
    dpt dpts =
  let acronym = dpt.Public_data.dpt_acronyme in
  let dpt_opt' =
    get_dpt ~acronym dpts
  in
  let state, dpt =
    match dpt_opt' with
    | None -> state, dpt
    | Some dpt' ->
      unify pos state dpt dpt'
  in
  let dpts =
    Public_data.AcronymMap.add
      acronym
      dpt
      dpts
  in
  state, dpts
