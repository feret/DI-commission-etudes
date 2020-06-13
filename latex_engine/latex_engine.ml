let latex_to_pdf ~input state =
  let state, options =
    Remanent_state.get_pdfgenerator_options state
  in
  match Remanent_state.get_pdfgenerator_engine state with
  | state, Public_data.PdfLatex ->
    let state, current =
      Safe_sys.getcwd __POS__ state
    in
    let input_file =
      match fst input with
      | "" -> snd input
      | x -> x^"/"^(snd input)
    in
    let rep,file_name =
      match String.rindex_opt input_file '/' with
      | Some i ->
        String.sub input_file 0 i,
        String.sub input_file (i+1) ((String.length input_file)-(i+1))
      | None -> "",input_file
    in
    let state =
      Safe_sys.chdir __POS__ state rep
    in
    let command =
      Printf.sprintf "pdflatex %s %s" options file_name
    in
    let state =
      Safe_sys.command __POS__ state command
    in
    let basename =
      Tools.basename file_name
    in
    let file1 =
      Printf.sprintf "%s.log" basename
    in
    let file2 =
      Printf.sprintf "%s.aux" basename
    in
    let state =
      Safe_sys.rm __POS__ state file1
    in
    let state =
      Safe_sys.rm __POS__ state file2
    in
    Safe_sys.chdir __POS__ state current
