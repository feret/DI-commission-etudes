type t =
  Public_data.compensation
    Public_data.CodeMap.t
    Public_data.YearMap.t
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

let empty = Public_data.LastNameMap.empty

let get_compensation ~firstname ~lastname ~year ~codecours
    compensations =
  match
    Public_data.LastNameMap.find_opt
      lastname
      compensations
  with
  | None -> None
  | Some map
    ->
    match
      Public_data.FirstNameMap.find_opt
        firstname
        map
    with
    | None -> None
    | Some map ->
      match
        Public_data.YearMap.find_opt
          year
          map
      with
      | None -> None
      | Some map ->
        Public_data.CodeMap.find_opt
          codecours
          map



let add_compensation
    unify pos state
    compensation compensations  =
  let codecours = compensation.Public_data.comp_codecours in
  let year = compensation.Public_data.comp_annee in
  let firstname = compensation.Public_data.comp_firstname in
  let lastname = compensation.Public_data.comp_lastname in
  let compensation_opt' =
    get_compensation ~codecours ~year ~firstname ~lastname compensations
  in
  let state, compensation =
    match compensation_opt' with
  | None -> state, compensation
  | Some compensation' ->
    unify pos state compensation compensation'
  in
  let map1 =
    match
      Public_data.LastNameMap.find_opt lastname compensations
    with
    | None -> Public_data.FirstNameMap.empty
    | Some map1 -> map1
  in
  let map2 =
    match
      Public_data.FirstNameMap.find_opt firstname map1
    with
    | None -> Public_data.YearMap.empty
    | Some map2 -> map2
  in
  let map3 =
    match
      Public_data.YearMap.find_opt year map2
    with
    | None -> Public_data.CodeMap.empty
    | Some map3 -> map3
  in
  state,
  Public_data.LastNameMap.add
    lastname
    (Public_data.FirstNameMap.add
       firstname
       (Public_data.YearMap.add
          year
          (Public_data.CodeMap.add
             codecours compensation
             map3)
          map2)
       map1)
    compensations
