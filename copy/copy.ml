let pdf_file x =
  Printf.sprintf "%s.%s" (Tools.basename x) "pdf"

let copy ~input_rep ~file_name ~output_rep state =
  let state =
    Remanent_state.warn
      __POS__
      (Printf.sprintf "COPY %s.%s -> %s.%s" input_rep file_name output_rep file_name)
      Exit
      state
  in
  let input_file =
    match input_rep with
    | "" -> file_name
    | _ -> Printf.sprintf "%s/%s" input_rep file_name
  in
  let state,save_rep  =
    Safe_sys.rec_mk_when_necessary __POS__ state output_rep
  in
  Safe_sys.cp __POS__ state input_file save_rep
