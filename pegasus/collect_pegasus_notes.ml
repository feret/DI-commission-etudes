(* Collect dens candidates from the data-bases *)
type pegasus_entry =
{
  firstname: string option;
  lastname: string option;
  year: string option;
  intitule: string option;
  produit: string option;
  code_helisa: string option ;
  nombre_etudiants: int option;
  control: (int*string) option;
  note: string option;
  validation: string option
}

let empty_pegasus_entry =
{
  firstname = None;
  lastname = None;
  year = None;
  intitule = None ;
  produit = None ;
  code_helisa = None;
  nombre_etudiants = None ;
  control = None ;
  note = None;
  validation = None;
}

let _ = empty_pegasus_entry.control , empty_pegasus_entry.nombre_etudiants, empty_pegasus_entry.intitule


let split a =
    if String.length a > 2 then
      let i = (String.length a -2)/2 in
      String.sub a 2 i
    else a

let update_product produit entry state =
let state = Remanent_state.warn __POS__ (Format.sprintf "PRODUCT -> %s " produit) Exit state in

        let code_helisa = split produit in
let state = Remanent_state.warn __POS__ (Format.sprintf "CODE HELISA -> %s " code_helisa) Exit state in
        let produit = Some produit in
        let code_helisa = Some code_helisa in
        state, {entry with code_helisa ; produit }

let update_controle a b entry state =
  let state = Remanent_state.warn __POS__ (Format.sprintf "CONTROLE -> %s,%s " a b) Exit state in
  try
    state, {entry with control = Some (int_of_string a,b)}
  with
    _ -> state, entry


let update_n_etu n entry state =
  let state = Remanent_state.warn __POS__ (Format.sprintf "N ETU -> %s " n) Exit state in
  try
    state, {entry with nombre_etudiants = Some (int_of_string n)}
  with
    _ -> state, entry


let convert entry state =
  let state, validation =
      match entry.validation
      with
        | None -> state, None
        | Some x ->
          begin
            match
              Public_data.get_validation x
            with
              | None ->
                Remanent_state.warn
                  __POS__
                  (Format.sprintf "Wrong validatio status in Helisa (%s)" x)
                  Exit
                  state, None
              | Some _ as y -> state, y
          end
  in
  state, {
    Public_data.pegasus_note_annee = Tools.unsome_string entry.year ;
    Public_data.pegasus_note_firstname = Tools.unsome_string entry.firstname ;
    Public_data.pegasus_note_lastname = Tools.unsome_string entry.lastname ;
    Public_data.pegasus_note = entry.note ;
    Public_data.pegasus_validation = validation ;
    Public_data.pegasus_note_code_helisa = Tools.unsome_string entry.code_helisa ;
    Public_data.pegasus_note_produit = split (Tools.unsome_string entry.produit)}

let convert entry state =
    let state, elt = convert entry state in
    let state =
        Remanent_state.warn __POS__
          (Format.sprintf "CONVERT NOTE: %s %s %s %s %s %s %s"
                elt.Public_data.pegasus_note_annee
elt.Public_data.pegasus_note_firstname
elt.Public_data.pegasus_note_lastname
(match elt.Public_data.pegasus_note with None -> "None" | Some x -> x)
(match elt.Public_data.pegasus_validation with None -> "None" | Some _ -> "Some")
elt.Public_data.pegasus_note_code_helisa
elt.Public_data.pegasus_note_produit
) Exit state in state, elt

let unify pos state a b  =
    let state, b1 =
        if a.Public_data.pegasus_note_annee <> b.Public_data.pegasus_note_annee
        then Remanent_state.warn pos "Distinct years" Exit state, false
        else state, true
    in
    let state, b2 =
        if a.Public_data.pegasus_note_firstname <> b.Public_data.pegasus_note_firstname
        then Remanent_state.warn pos "Distinct first names" Exit state, false
        else state, true
    in
    let state, b3 =
        if a.Public_data.pegasus_note_lastname <> b.Public_data.pegasus_note_lastname
        then Remanent_state.warn pos "Distinct last names" Exit state, false
        else state, true
    in
    let state, b4 =
        if a.Public_data.pegasus_note_code_helisa <> b.Public_data.pegasus_note_code_helisa
        then Remanent_state.warn pos  "Distinct Helisa code" Exit state, false
        else state, true
    in
    let state, b5 =
        if a.Public_data.pegasus_note_produit<> b.Public_data.pegasus_note_produit
        then Remanent_state.warn pos "Distinct products" Exit state, false
        else state, true
    in
    if b1 && b2 && b3 && b4 && b5
    then
      let state, b =
      match a.Public_data.pegasus_note, b.Public_data.pegasus_note with
      | Some x, Some y when x=y -> state,b
      | None, _ -> state,b
      | Some x, None -> state, {b with Public_data.pegasus_note = Some x}
      | Some _, Some _ ->
          Remanent_state.warn pos "Incompatible notes" Exit state, b
      in
      let state, b =
      match a.Public_data.pegasus_validation, b.Public_data.pegasus_validation with
      | Some x, Some y when x=y -> state,b
      | None, _ -> state,b
      | Some x, None -> state, {b with Public_data.pegasus_validation = Some x}
      | Some _, Some _ ->
          Remanent_state.warn pos "Incompatible validation status" Exit state, b
    in
      state, b
  else
    state, b

    let update_note _titre nom prenom _id
    _ref_externe note entry state  =
        let state = Remanent_state.warn __POS__ (Format.sprintf "NOTE -> %s %s %s %s %s %s" _titre nom prenom _id _ref_externe note) Exit state in
        let state, entry = convert {entry with note = Some note ; firstname = Some prenom ; lastname = Some nom } state
        in
        Remanent_state.Collector_pegasus_notes.add unify __POS__ entry state


let get
      compute_repository
      event_opt
      ?repository
      ?prefix
      ?file_name
      state
       =
      let state = Remanent_state.open_event_opt event_opt state in
      let state, repository =
            match repository with
            | Some a -> state, a
            | None ->
              let state, a =
                compute_repository state
              in
              state, a
            in
            let event = Some (Profiling.Scan_csv_files (repository,"")) in
            let state = Remanent_state.open_event_opt event state in
            let state, files_list =
                Scan_repository.get_list_of_files
                  ~repository ?prefix ?file_name state
            in
            let state =
              List.fold_left
                  (fun state file ->
                    let event = Some (Profiling.Scan_csv_files (fst file,snd file)) in
                    let state = Remanent_state.open_event_opt event state in
                    let _ =
                      Format.printf
                        "Scanning file : %s %s @." (fst file) (snd file)
                    in
                    let _ =
                      Format.print_newline ()
                    in
                    let _ =
                      Format.print_flush ()
                    in
                    let file =
                      if fst file = ""
                      then
                        snd file
                      else
                        Printf.sprintf "%s/%s" (fst file) (snd file)
                    in
                    let csv =
                       Xls_support.open_xlsx file
                    in
                    let rec scan list entry (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let state, entry =
                              begin
                                match h with
                                | "Intitulé produit":: _
                                | "TITRE"::_
                                | "Ensemble Cible"::_ ->
                                    state, entry
                                | "PRODUIT"::prod::_->
                                      update_product prod entry state
                                | "Nombre d'étudiants" ::int::_ ->
                                      update_n_etu int entry state
                                | "CONTROLE"::a::b::_ ->
                                      update_controle a b entry state
                                | titre::nom::prenom::id::ref_externe::note::_
                                    ->
                                      update_note
                                          titre nom prenom id
                                          ref_externe note entry state, entry
                                | _ -> state, entry
                            end
                          in
                          scan t entry state
                    in
                    let state =
                      List.fold_left
                          (fun state row ->
                              let state = List.fold_left
                                  (fun state elt ->
                                      Remanent_state.warn __POS__ (Format.sprintf "%s," elt) Exit state)
                                  state row in
                              Remanent_state.warn __POS__ "ENDLINE" Exit state)
                          state csv
                    in
                    let state = scan csv empty_pegasus_entry state in
                    let state = Remanent_state.close_event_opt event state in
                     state

                  ) state files_list
            in
            let state = Remanent_state.close_event_opt event state in
            let state = Remanent_state.close_event_opt event_opt state in
            state

let event_opt = Some (Profiling.Collect_pegasus_notes)
let compute_repository = Remanent_state.Collector_pegasus_notes.get_repository

let get_pegasus_notes = get compute_repository event_opt

let event_opt = Some (Profiling.Collect_pegasus_validations)
let compute_repository = Remanent_state.Collector_pegasus_validations.get_repository

let get_pegasus_validations = get compute_repository event_opt
