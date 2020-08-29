type t =
  Public_data.cursus
    Public_data.YearMap.t
    Public_data.DptOptMap.t
    Public_data.LevelMap.t

let empty = Public_data.LevelMap.empty

let get_cursus ~strong ~level ?dpt ~year cursus =
  let level =
    Special_char.lowercase level
  in
  let dpt =
    Tools.map_opt Special_char.lowercase dpt
  in
  match
    Public_data.LevelMap.find_opt
      level
      cursus
  with
  | None -> None
  | Some a ->
    let yearmap =
      (match
         Public_data.DptOptMap.find_opt
           dpt
           a
       with
       | None -> Public_data.YearMap.empty
       | Some a -> a)
    in
    if strong
    then
      Public_data.YearMap.find_opt
        year yearmap
    else
      Public_data.YearMap.fold
        (fun year' cursus output ->
           if compare year' year <= 0
           then Some cursus
           else output)
        yearmap None

let get_cursus ~strong ~level ?dpt ~year cursus =
  match
    get_cursus ~strong ~level ?dpt ~year cursus, dpt
  with
  | None, Some _ ->
    get_cursus ~strong ~level ~year cursus
  | x, _ -> x

let add_cursus
    unify pos state
    cursus_elt cursus_map =
  let level = cursus_elt.Public_data.cursus_niveau in
  let year = cursus_elt.Public_data.cursus_annee_academique in
  let dpt = cursus_elt.Public_data.cursus_dpt in
  let cursus_opt' =
    get_cursus
      ~strong:true ~level ?dpt ~year cursus_map
  in
  let state, cursus_elt =
    match cursus_opt' with
    | None -> state, cursus_elt
    | Some cursus_elt' ->
        unify pos state cursus_elt cursus_elt'
  in
  let mapa =
    match
      Public_data.LevelMap.find_opt
        level
        cursus_map
    with
    | None -> Public_data.DptOptMap.empty
    | Some a -> a
  in
  let mapb =
    match
      Public_data.DptOptMap.find_opt
        dpt
        mapa
    with
    | None -> Public_data.YearMap.empty
    | Some a -> a
  in
  state,
  Public_data.LevelMap.add
    level
    (Public_data.DptOptMap.add
       dpt
       (Public_data.YearMap.add
          year
          cursus_elt
          mapb)
       mapa)
    cursus_map

let add_cursus unify pos state
    cursus_elt cursus_map =
  let state, output =
      add_cursus unify pos state
        cursus_elt cursus_map
  in
  match cursus_elt.Public_data.cursus_dpt with
  | None -> state, output
  | Some _ ->
    let cursus_elt = {cursus_elt with Public_data.cursus_dpt = None} in
    add_cursus unify pos state
      cursus_elt output

let get_cursus = get_cursus ~strong:false
