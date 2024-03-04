type t =
    Public_data.note_pegasus
      Public_data.FirstNameMap.t
        Public_data.LastNameMap.t
          Public_data.CodeMap.t
            Public_data.YearMap.t

let empty =
      Public_data.YearMap.empty

let get_pegasus_note ~code ~year ~firstname ~lastname notes =
  let code =
    String.lowercase_ascii code
  in
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in
  match
    Public_data.YearMap.find_opt
      year
      notes
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
         | Some b ->
             begin
               match
                  Public_data.LastNameMap.find_opt
                    lastname
                    b
                with
                  | None -> None
                  | Some c ->
                      Public_data.FirstNameMap.find_opt
                        firstname c
             end
      end

let add_pegasus_note
    unify pos state
    note notes =
  let code = note.Public_data.pegasus_note_code_helisa in
  let year = note.Public_data.pegasus_note_annee in
  let firstname = note.Public_data.pegasus_note_firstname in
  let lastname = note.Public_data.pegasus_note_lastname in
  let note' = get_pegasus_note ~code ~year ~firstname ~lastname notes  in
  let state, note =
    match note' with
    | None -> state, note
    | Some note' ->
        unify pos state note note'
  in
  let notes =
    let old_year =
      match
        Public_data.YearMap.find_opt
          year
          notes
      with
      | Some map -> map
      | None -> Public_data.CodeMap.empty
    in
    let old_code =
      match
        Public_data.CodeMap.find_opt
          code
          old_year
      with
        | Some map -> map
        | None -> Public_data.LastNameMap.empty
    in
    let old_last =
      match
        Public_data.LastNameMap.find_opt
          lastname
          old_code
      with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
    in
    Public_data.YearMap.add year
      (Public_data.CodeMap.add code
        (Public_data.LastNameMap.add lastname
            (Public_data.FirstNameMap.add firstname note old_last)
            old_code)
      old_year) notes
  in
  state, notes
