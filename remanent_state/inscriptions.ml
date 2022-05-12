type t =
  Public_data.inscription
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t
    Public_data.YearMap.t
    Public_data.DptOptMap.t
    Public_data.LevelMap.t

let empty = Public_data.LevelMap.empty

let get_inscription ~level ?dpt ~year ~firstname ~lastname cursus =
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
    let lastnamemap =
        match
          Public_data.YearMap.find_opt
          year yearmap
        with
        | None -> Public_data.LastNameMap.empty
        | Some a -> a
      in
    let firstnamemap =
      match
        Public_data.LastNameMap.find_opt
          lastname
          lastnamemap
      with
      | None -> Public_data.FirstNameMap.empty
      | Some a -> a
    in
    Public_data.FirstNameMap.find_opt firstname firstnamemap

let add_inscription
    unify pos state
    inscription_elt inscription_map =
  let level = inscription_elt.Public_data.inscription_niveau in
  let year = inscription_elt.Public_data.inscription_annee_academique in
  let dpt = inscription_elt.Public_data.inscription_dpt in
  let lastname = inscription_elt.Public_data.inscription_nom in
  let firstname = inscription_elt.Public_data.inscription_prenom in

  let inscription_opt' =
    get_inscription
      ~level ?dpt ~year ~lastname ~firstname inscription_map
  in
  let state, inscription_elt =
    match inscription_opt' with
    | None -> state, inscription_elt
    | Some inscription_elt' ->
        unify pos state inscription_elt inscription_elt'
  in
  let mapa =
    match
      Public_data.LevelMap.find_opt
        level
        inscription_map
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
    | None -> Public_data.LastNameMap.empty
    | Some a -> a
  in
  let mapd =
    match
      Public_data.LastNameMap.find_opt
        year
        mapc
    with
    | None -> Public_data.FirstNameMap.empty
    | Some a -> a
  in
  state,
  Public_data.LevelMap.add
    level
    (Public_data.DptOptMap.add
       dpt
       (Public_data.YearMap.add
          year
          (Public_data.LastNameMap.add
            lastname
              (Public_data.FirstNameMap.add
                  firstname
                  inscription_elt
                  mapd)
            mapc)
          mapb)
       mapa)
    inscription_map
