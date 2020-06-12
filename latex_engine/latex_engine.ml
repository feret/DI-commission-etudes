let latex_to_pdf ~input state =
  let state, options =
    Remanent_state.get_pdfgenerator_options state
  in
  match Remanent_state.get_pdfgenerator_engine state with
  | state, Public_data.PdfLatex ->
    let input_file =
      match fst input with
      | "" -> snd input
      | x -> x^"/"^(snd input)
    in
    let command =
      Printf.sprintf "pdflatex %s %s" options input_file
    in
    Safe_sys.command __POS__ state command
