(* Collect dens candidates from the data-bases *)
type pegasus_entry =
{
  firstname: string option;
  lastname: string option;
  promotion: string option;
  year: string option;
  ects: float option;
  libelle: string option;
  code: string option;
  tutor_firstname: string option;
  tutor_lastname: string option;
  student_number: string option;
  ine: string option
}

let empty_pegasus_entry =
{
  firstname=None;
  lastname=None;
  promotion=None;
  year=None;
  ects=None;
  libelle=None;
  code=None;
  tutor_firstname=None;
  tutor_lastname=None;
  student_number=None;
  ine=None;
}

let _ = empty_pegasus_entry
let _ = empty_pegasus_entry.ine, empty_pegasus_entry.student_number, empty_pegasus_entry.tutor_firstname, empty_pegasus_entry.code, empty_pegasus_entry.libelle, empty_pegasus_entry.ects, empty_pegasus_entry.year,empty_pegasus_entry.promotion, empty_pegasus_entry.lastname, empty_pegasus_entry.firstname

(*Student: CARENINI Gaia Student number: 2022n00416 INE number: 203191976DF Tutor: SENELLART Pierre*)

let decompose_name l =
  let rec aux l lastname =
    match l with
      | [] -> List.rev lastname, l
      | h::t ->
        if Special_char.uppercase h = h
        then
          aux t  (h::lastname)
        else
          List.rev lastname, l
 in
 let firstname, lastname = aux l [] in
 let firstname = String.concat " " firstname in
 let lastname = String.concat " " lastname in
 lastname, firstname

let rec fetch_name l acc =
  match l with
    | "Student:"::_
    | "Student"::"number:"::_
    | "INE"::"number:"::_
    | "Tutor:"::_
    | [] -> List.rev acc, l
    | h::t -> fetch_name t (h::acc)

let fetch_name l =
  let name, tail = fetch_name l [] in
  let lastname, firstname = decompose_name name in
  tail, lastname, firstname

let update_student bloc entry state =
    let l = String.split_on_char ' ' bloc in
    let rec aux l entry =
          match l with
          | "Student:"::t ->
             let t, lastname, firstname = fetch_name t in
             let () = Format.printf "%s %s @." lastname firstname in
             aux t {entry with lastname = Some lastname; firstname = Some firstname}
          | "Student"::"number:"::n::t ->
              let () = Format.printf "%s@." n in

             aux t {entry with student_number = Some n; promotion = Some (String.sub n 0 4)}
          | "INE"::"number:"::n::t ->
            let () = Format.printf "%s@." n in

             aux t {entry with ine = Some n}
          | "Tutor"::t ->
          let t, tutor_lastname, tutor_firstname = fetch_name t in
          let () = Format.printf "%s %s @." tutor_lastname tutor_firstname in
          aux t {entry with tutor_lastname = Some tutor_lastname ; tutor_firstname = Some tutor_firstname}
          | _ -> entry
    in
    let entry = aux l entry in
    entry, state

let update_year year entry state =
  let l = String.split_on_char ' ' year in
  match l with "Academic"::"year"::y::_ -> {entry with year = Some y}, state
      | _ -> entry, state

let convert entry =
{
  Public_data.pe_firstname = Tools.unsome_string entry.firstname;
  Public_data.pe_lastname = Tools.unsome_string entry.lastname;
  Public_data.pe_promotion = Tools.unsome_string entry.promotion;
  Public_data.pe_year = Tools.unsome_string entry.year;
  Public_data.pe_ects = entry.ects ;
  Public_data.pe_libelle = Tools.unsome_string entry.libelle;
  Public_data.pe_code = "";
  Public_data.pe_tutor_firstname = Tools.unsome_string entry.tutor_firstname;
  Public_data.pe_tutor_lastname = Tools.unsome_string entry.tutor_lastname;
  Public_data.pe_student_number = "";
  Public_data.pe_ine = "";
}
let update_diploma diploma entry (state:Remanent_state.t) =
  let code, libelle =
    let l = String.split_on_char ' ' diploma in
    match l with
    | h::t -> h, String.concat " " t
    | [] -> "",""
  in
  let code, libelle = Some code, Some libelle in
  Remanent_state.Collector_pedagogical_registrations.add
        (fun _ state a _ -> state,a) __POS__
        (convert {entry with libelle ; code }) state

let update_course course ects entry (state:Remanent_state.t) =
    let code, libelle =
       let l = String.split_on_char ' ' course in
       match l with
        | h::t -> h, String.concat " "  t
        | [] -> "",""
    in
    let ects = float_of_string ects in
    let code, libelle, ects = Some code, Some libelle, Some ects in
    Remanent_state.Collector_pedagogical_registrations.add
                (fun _ state a _ -> state,a) __POS__
                (convert {entry with libelle ; ects ; code }) state

let event_opt = Some (Profiling.Collect_pegasus_pedagogical_registrations)
let compute_repository = Remanent_state.Collector_pedagogical_registrations.get_repository



let get_pegasus_pedagogical_registrations
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
                            let entry, state =
                              begin
                                match h with
                                | ""::"LEARNING AGREEMENT"::_ ->
                                    let () = Format.printf "LEARNING AGREEMENT @." in
                                    entry, state
                                | ""::""::academic::_ ->
                                      let () = Format.printf "ACADEMIC %s @." academic in
                                      update_year academic entry state
                                | ""::diploma::""::""::""::""::""::_ ->
                                       let () = Format.printf "DIPLOMA %s @."  diploma in
                                       entry, update_diploma diploma entry state
                                | ""::course::""::""::ects::_ ->
                                      let () = Format.printf "COURSE %s @." course in
                                      entry,  update_course course ects entry state
                                | bloc::_ -> let () = Format.printf "BLOC %s @." bloc in update_student bloc entry state
                                | [] -> entry, state
        end
                          in
                          scan t entry state
                    in
                    let state = scan csv empty_pegasus_entry state in
                    let state = Remanent_state.close_event_opt event state in
                     state

                  ) state files_list
            in
            let state = Remanent_state.close_event_opt event state in
            let state = Remanent_state.close_event_opt event_opt state in
            state
