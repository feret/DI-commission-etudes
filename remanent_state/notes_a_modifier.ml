type t =
  Public_data.note_a_modifier list
    Public_data.YearMap.t
    Public_data.CodeMap.t
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

let empty = Public_data.LastNameMap.empty

let get_note_a_modifier ~firstname ~lastname ~code ~year
    cours_a_ajouter =
  match
    Public_data.LastNameMap.find_opt
      lastname
      cours_a_ajouter
  with
  | None -> []
  | Some a ->
    begin
      match
        Public_data.FirstNameMap.find_opt
          firstname
          a
      with
      | None -> []
      | Some a ->
        begin
          match
            Public_data.CodeMap.find_opt
              code
              a
          with
          | None -> []
          | Some a ->
            match
              Public_data.YearMap.find_opt
                year
                a
            with
            | None -> []
            | Some a -> a
        end
    end

let add_note_a_modifier
  _pos state
    note_elt note_map =
  let lastname = note_elt.Public_data.notetm_nom in
  let firstname = note_elt.Public_data.notetm_prenom in
  let code = note_elt.Public_data.notetm_code in
  let year = note_elt.Public_data.notetm_annee in
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

  let old =
    match
      Public_data.YearMap.find_opt
        year
        mapc
    with
    | None -> []
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
           (note_elt::old)
           mapc)
        mapb)
     mapa)
  note_map
