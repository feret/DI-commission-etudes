(* Collect dens candidates from the data-bases *)
type pegasus_entry =
{
  firstname: string option;
  lastname: string option;
  year: string option;
  ects: float option;
  libelle: string option;
  code_helisa: string option;
  code_gps: string option;
  tutor_firstname: string option;
  tutor_lastname: string option;
  student_number: string option;
  ine: string option;
  libelle_gps: string option ;
  teachers: (string * string) list ;
}

let empty_pegasus_entry =
{
  firstname=None;
  lastname=None;
  year=None;
  ects=None;
  libelle=None;
  code_helisa=None;
  code_gps=None;
  tutor_firstname=None;
  tutor_lastname=None;
  student_number=None;
  ine=None;
  libelle_gps = None;
  teachers = [];
}

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
 let lastname, firstname = aux l [] in
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
    let () = Format.printf "UPDATE STUDENT %s @." bloc in
    let l = String.split_on_char ' ' bloc in
    let rec aux l entry =
          match l with
          | "Student:"::t ->
             let t, lastname, firstname = fetch_name t in
             aux t {entry with lastname = Some lastname; firstname = Some firstname}
          | "Student"::"number:"::n::t ->
             aux t {entry with student_number = Some n}
          | "INE"::"number:"::n::t ->
             aux t {entry with ine = Some n}
          | "Tutor"::t ->
          let t, tutor_lastname, tutor_firstname = fetch_name t in
          aux t {entry with tutor_lastname = Some tutor_lastname ; tutor_firstname = Some tutor_firstname}
          | _ -> entry
    in
    let entry = aux l entry in
    entry, state

let update_year year entry state =
  let () = Format.printf "UPDATE YEAR %s @." year in
  let l = String.split_on_char ' ' year in
  match l with "Academic"::"year"::y::_ -> {entry with year = Some y}, state
      | _ -> entry, state

let add unify pos c state =
   let () = Format.printf "UPDATE ADD @." in
   Remanent_state.Collector_pedagogical_registrations.add unify pos c state

let convert entry state =
  state, {
  Public_data.pe_firstname = Tools.unsome_string entry.firstname;
  Public_data.pe_lastname = Tools.unsome_string entry.lastname;
  Public_data.pe_year = Tools.unsome_string entry.year;
  Public_data.pe_ects = entry.ects ;
  Public_data.pe_libelle =
    begin
      match entry.libelle_gps with
        | None -> Tools.unsome_string entry.libelle
        | Some x -> x
    end;
  Public_data.pe_code_helisa = Tools.unsome_string entry.code_helisa;
  Public_data.pe_code_gps = entry.code_gps;

  Public_data.pe_tutor_firstname = Tools.unsome_string entry.tutor_firstname;
  Public_data.pe_tutor_lastname = Tools.unsome_string entry.tutor_lastname;
  Public_data.pe_student_number = Tools.unsome_string entry.student_number;
  Public_data.pe_ine = Tools.unsome_string entry.ine;
  Public_data.pe_teachers = entry.teachers;
}


let update_diploma diploma entry (state:Remanent_state.t) =
  let () = Format.printf "UPDATE DIPLOMA %s @." diploma in
  let code, libelle =
    let l = String.split_on_char ' ' diploma in
    match l with
    | h::t -> h, String.concat " " t
    | [] -> "",""
  in
  let code_gps = entry.code_gps in
  let code_helisa, libelle = Some code, Some libelle in
  let state, entry = convert {entry with libelle ; code_helisa ; code_gps } state in
  add
        (fun _ state a _ -> state,a) __POS__
        entry state

let get_teachers entry =
  let list = entry.Public_data.pegasus_profs in
  match list with None -> []
    | Some list ->
  let n = String.length list in
  let rec split deb k acc =
      if k=n then ((String.sub list deb (k-deb))::acc) else
      if (k+1<n && String.sub list k 2 = "et") then
         split (k+2) (k+2) ((String.sub list deb (k-deb+1))::acc)
      else if
         (k+2<n && String.sub list k 3 = "and") then
         split (k+3) (k+3) ((String.sub list deb (k-deb+1))::acc)
      else split deb (k+1) acc
  in
  let l = split 0 0 [] in
    List.rev_map
      (fun a -> decompose_name (String.split_on_char ' ' a))
      l


let update_course course ects entry (state:Remanent_state.t) =
  let () = Format.printf "UPDATE COURSE %s %s @." course ects in
    let codehelisa, libelle =
       let l = String.split_on_char ' ' course in
       match l with
        | h::"-"::t | h::t -> h, String.concat " "  t
        | [] -> "",""
    in
    let state, year =
        match entry.year with
          | None ->
              Remanent_state.warn
                  __POS__
                  "Year is missing"
                  Exit
                  state, ""
          | Some y -> state, y
    in
    let ects = float_of_string ects in
    let code_helisa, libelle, ects = Some codehelisa, Some libelle, Some ects in
    let state, pegasus_entry =
        Remanent_state.get_course_in_pegasus ~codehelisa ~year state
    in
    match pegasus_entry with
      | None ->
        let state =
          Remanent_state.warn
          __POS__
          (Format.sprintf "Pegasus entry is missing %s %s" codehelisa year)
          Exit
          state
        in
        let state, entry = convert {entry with libelle ; ects ; code_helisa } state in
          add
                (fun _ state a _ -> state,a) __POS__
                entry state
      | Some pegasus_entry ->
        let teachers = get_teachers pegasus_entry in
        let code_gps = pegasus_entry.Public_data.pegasus_codegps in
        let libelle_gps = Some pegasus_entry.Public_data.pegasus_libelle in
        let state, entry = convert {entry with libelle ; ects ; code_helisa ; code_gps ; teachers ;  libelle_gps } state in
          add
                (fun _ state a _ -> state,a) __POS__
                entry state

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
                        Scan_xlss_files.get_csv file
                    in
                    let () = Format.printf "CSV %s %i@." file (List.length csv) in
                    let rec scan list entry (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let entry, state =
                              begin
                                match h with
                                | ""::"LEARNING AGREEMENT"::_ ->
                                    entry, state
                                | ""::""::academic::_ ->
                                      update_year academic entry state
                                | ""::diploma::""::""::""::""::""::_ ->
                                       entry, update_diploma diploma entry state
                                | ""::course::""::""::ects::_ ->
                                      entry,  update_course course ects entry state
                                | bloc::_ ->  update_student bloc entry state
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
