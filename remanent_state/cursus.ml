type t =
  Public_data.cursus
    Public_data.CodeOptMap.t
    Public_data.YearMap.t
    Public_data.DptOptMap.t
    Public_data.LevelMap.t

let empty = Public_data.LevelMap.empty

let get_cursus ~strong ~level ?dpt ?gpscode ~year cursus =
  let level =
    Special_char.lowercase level
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
      let codemap =
        match
          Public_data.YearMap.find_opt
          year yearmap
        with
        | None -> Public_data.CodeOptMap.empty
        | Some a -> a
      in
      Public_data.CodeOptMap.find_opt
          gpscode
          codemap
    else
      Public_data.YearMap.fold
        (fun year' cursus output ->
           if compare year' year <= 0
           then
              match Public_data.CodeOptMap.find_opt gpscode cursus
              with
              | Some cursus -> Some cursus
              | None -> output
           else output)
        yearmap None

let get_cursus ~strong ~strong_dpt  ~level ?dpt ?gpscode ~year cursus =
  match
    get_cursus ~strong ~level ?dpt ?gpscode ~year cursus, dpt
  with
  | None, Some _ ->
    if strong_dpt then
      None
    else
      get_cursus ~strong ~level ~year ?gpscode cursus
  | x, _ -> x

let add_cursus
    unify pos state
    cursus_elt cursus_map =
  let level = cursus_elt.Public_data.cursus_niveau in
  let year = cursus_elt.Public_data.cursus_annee_academique in
  let dpt = cursus_elt.Public_data.cursus_dpt in
  let code = cursus_elt.Public_data.cursus_gps in
  let cursus_opt' =
    get_cursus
      ~strong:true ~strong_dpt:true ~level ?dpt ~year cursus_map
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
  let mapc =
    match
      Public_data.YearMap.find_opt
        year
        mapb
    with
    | None -> Public_data.CodeOptMap.empty
    | Some a -> a
  in
  state,
  Public_data.LevelMap.add
    level
    (Public_data.DptOptMap.add
       dpt
       (Public_data.YearMap.add
          year
          (Public_data.CodeOptMap.add
            code
            cursus_elt
            mapc)
          mapb)
       mapa)
    cursus_map

let get_cursus = get_cursus ~strong:false ~strong_dpt:false
