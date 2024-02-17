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
  tutor: string option;
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
  tutor=None;
  student_number=None;
  ine=None;
}

let _ = empty_pegasus_entry
let _ = empty_pegasus_entry.ine, empty_pegasus_entry.student_number, empty_pegasus_entry.tutor, empty_pegasus_entry.code, empty_pegasus_entry.libelle, empty_pegasus_entry.ects, empty_pegasus_entry.year,empty_pegasus_entry.promotion, empty_pegasus_entry.lastname, empty_pegasus_entry.firstname

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
                    let () = Format.printf "NROWS: %i @." (List.length csv) in 
                    let () =
                          List.iter
                            (fun row ->
                                List.iter (Format.printf "%s,&,") row;
                                Format.printf "@.") csv
                    in
                    let state = Remanent_state.close_event_opt event state in
                     state

                  ) state files_list
            in
            let state = Remanent_state.close_event_opt event state in
            let state = Remanent_state.close_event_opt event_opt state in
            state
