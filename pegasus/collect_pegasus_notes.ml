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
  let a = Tools.remove_space_from_string a in 
  let n = String.length a in
  let rec aux i =
    if i >= n then None
    else
      if String.get a i = 'U'
      then Some i
      else aux (i+1)
  in
  match aux 0 with
    | None -> a,""
    | Some i ->
        let y = String.sub a 0 i in
        let y = if i = 4 then "20"^(String.sub a 0 2)
                else if i = 2 then "20"^y else y in
        let size = n-i in
        let res = String.sub a i (n-i) in
        let x = String.get res 0 in
        let rec aux k l =
          if k>size/2 || k=size then l
          else if String.get res k = x then aux (k+1) (k::l)
          else aux (k+1) l
        in
        let l = aux 1 [] in
        let rec aux l =
          match l with
            | [] -> a,""
            | h::t ->
                let r = String.sub res 0 h in
                if r = String.sub res h h
                then y,r
                else aux t
        in aux l

let update_product produit entry pos state =
     let year, code_helisa = split produit in
     let year = Some year in
        let produit = Some produit in
        let code_helisa = Some code_helisa in
        state, {entry with code_helisa ; produit ; year}, pos 

let update_controle a b entry pos state =
  try
    state, {entry with control = Some (int_of_string a,b)}, pos 
  with
    _ -> state, entry, pos


let update_n_etu n entry pos state =
  try
    state, {entry with nombre_etudiants = Some (int_of_string n)}, pos
  with
    _ -> state, entry, pos 


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
                  (Format.sprintf "Wrong validation status in Helisa (%s)" x)
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
    Public_data.pegasus_note_produit = Tools.unsome_string entry.produit; }

let unify pos state a b  =
    let state, b1 =
        if a.Public_data.pegasus_note_annee <> b.Public_data.pegasus_note_annee
        then Remanent_state.warn pos
               (Format.sprintf "Distinct years (%s)/(%s)" a.Public_data.pegasus_note_annee b.Public_data.pegasus_note_annee)Exit state, false
        else state, true
    in
    let state, b2 =
        if a.Public_data.pegasus_note_firstname <> b.Public_data.pegasus_note_firstname
        then Remanent_state.warn pos
                (Format.sprintf "Distinct first names (%s)/%s"
                    a.Public_data.pegasus_note_firstname b.Public_data.pegasus_note_firstname) Exit state, false
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
    let state, _b5 =
        if a.Public_data.pegasus_note_produit<> b.Public_data.pegasus_note_produit
        then Remanent_state.warn pos
                  (Format.sprintf "Distinct products (%s/%s)"
                        a.Public_data.pegasus_note_produit
                        b.Public_data.pegasus_note_produit)
                        Exit state, false
        else state, true
    in
    if b1 && b2 && b3 && b4 (*&& b5*)
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
        let note = match note with "" -> None | _ -> Some note in
        let state, entry = convert {entry with note ; firstname = Some prenom ; lastname = Some nom } state
        in
        Remanent_state.Collector_pegasus_notes.add unify __POS__ entry state

let update_validation _titre nom prenom _id
            _ref_externe etat entry state  =
                let validation = match etat with "" -> None | _ -> Some etat in
                let state, entry = convert {entry with validation ; firstname = Some prenom ; lastname = Some nom } state
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
                        "Scanning file: %s %s @." (fst file) (snd file)
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
                    let state,csv_opt =
                       Scan_xlss_files.get_csv file state
                    in
                    let csv =
                        match csv_opt with
                          | None -> []
                          | Some l -> l
                    in
                    let rec scan list entry pos (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let state, entry, pos  =
                              begin
                                match h with
                                | "Cycle"::_
                                | "Intitulé produit":: _
                                | "Ensemble"::_
                                | "Ensemble Cible"::_ ->
                                    state, entry, pos 
                                | "PRODUIT"::prod::_
                                | "ID UV"::prod::_ ->
                                      update_product prod entry pos state
                                | "Effectif"::int::_
                                | "Nombre d'étudiants" ::int::_ ->
                                      update_n_etu int entry pos state
                                | "CONTROLE"::a::b::_ ->
                                      update_controle a b entry pos state
                                | "TITRE"::_::_::_::_::_::a::_ 
                                when String.length a > 10 && String.sub a 5 4 = "NOTE" 
                                   ->    state, entry, 7 
                                | "TITRE"::_ -> state, entry, 6 
                                | titre::nom::prenom::id::ref_externe::note::q 
                                        ->
                                    let note = 
                                        if pos = 7 then match q with note'::_ -> note' | [] -> note 
                                        else note in 
                                          update_note
                                                titre nom prenom id
                                                ref_externe note entry state, entry, pos 

                                | _ -> state, entry, pos
                            end
                          in
                          scan t entry pos state
                    in
                    let state = scan csv empty_pegasus_entry 6 state in
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
                        "Scanning file: %s %s @." (fst file) (snd file)
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
                    let state,csv_opt =
                       Scan_xlss_files.get_csv file state
                    in
                    let csv =
                        match csv_opt with
                          | None -> []
                          | Some l -> l
                    in
                    let rec scan list entry pos (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let state, entry, pos  =
                              begin
                                match h with
                                | "Cycle"::_
                                | "Intitulé produit":: _
                                | "TITRE"::_
                                | "Ensemble"::_
                                | "Ensemble Cible"::_ ->
                                    state, entry, pos 
                                | "PRODUIT"::prod::_
                                | "ID UV"::prod::_ ->
                                      update_product prod entry pos state
                                | "Effectif"::int::_
                                | "Nombre d'étudiants" ::int::_ ->
                                      update_n_etu int entry pos state
                                | "CONTROLE"::a::b::_ ->
                                      update_controle a b entry pos state
                                | titre::nom::prenom::etat::id::ref_externe::_
                                              ->
                                                update_validation
                                                    titre nom prenom id
                                                    ref_externe etat entry state, entry, pos 
                                | _ -> state, entry, pos 
                            end
                          in
                          scan t entry pos state
                    in
                    let state = scan csv empty_pegasus_entry 6 state in
                    let state = Remanent_state.close_event_opt event state in
                     state

                  ) state files_list
            in
            let state = Remanent_state.close_event_opt event state in
            let state = Remanent_state.close_event_opt event_opt state in
            state


let event_opt = Some (Profiling.Collect_pegasus_validations)
let compute_repository = Remanent_state.Collector_pegasus_validations.get_repository

let get_pegasus_validations = get compute_repository event_opt
