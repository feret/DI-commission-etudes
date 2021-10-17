type t =
  Public_data.note_a_modifier
    Public_data.YearMap.t
    Public_data.CodeMap.t
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

let empty = Public_data.LastNameMap.empty

let get_note_a_modifier ~firstname ~lastname ~code ~year
    (cours_a_ajouter:t) =
  match
    Public_data.LastNameMap.find_opt
      lastname
      cours_a_ajouter
  with
  | None -> None
  | Some a ->
    begin
      match
        Public_data.FirstNameMap.find_opt
          firstname
          a
      with
      | None -> None
      | Some a ->
        begin
          match
            Public_data.CodeMap.find_opt
              code
              a
          with
          | None -> None
          | Some a ->
              Public_data.YearMap.find_opt
                year
                a
        end
    end

let add_note_a_modifier
    _warn unify
    pos state  note_elt note_map =
  let lastname = note_elt.Public_data.notetm_nom in
  let firstname = note_elt.Public_data.notetm_prenom in
  let code = note_elt.Public_data.notetm_code in
  let year = note_elt.Public_data.notetm_annee in
  let note_elt_opt' =
    get_note_a_modifier ~code ~year ~firstname ~lastname note_map
  in
  let state, note_elt =
    match note_elt_opt' with
    | None -> state, note_elt
    | Some note_elt'  ->
      unify pos state note_elt note_elt'
  in
  let mapa =
    match
      Public_data.LastNameMap.find_opt
        lastname
        note_map
    with
    | None -> Public_data.FirstNameMap.empty
    | Some a -> a
  in
  let mapb =
    match
      Public_data.FirstNameMap.find_opt
        firstname
        mapa
    with
    | None -> Public_data.CodeMap.empty
    | Some a -> a
  in
  let mapc =
    match
      Public_data.CodeMap.find_opt
        code
        mapb
    with
    | None -> Public_data.YearMap.empty
    | Some a -> a
  in
  state,
  Public_data.LastNameMap.add
    lastname
    (Public_data.FirstNameMap.add
       firstname
       (Public_data.CodeMap.add
          code
          (Public_data.YearMap.add
             year
             note_elt
             mapc)
          mapb)
       mapa)
    note_map
