let latex_to_pdf ?rev ~input state =
  let rev =
    match rev with
    | None | Some false -> false
    | Some true -> true
  in
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
    let state =
      if rev then
        let command =
          Printf.sprintf "pdftk %s.pdf cat 1-endeast output %s.tmp"
            basename basename
        in
        let state =
          Safe_sys.command __POS__ state command
        in
        let command =
          Printf.sprintf "pdftk %s.tmp cat end-1 output %s.pdf"
            basename basename
        in
        let state =
          Safe_sys.command __POS__ state command
        in
        let state =
          Safe_sys.rm __POS__ state (Printf.sprintf "%s.tmp" basename)
        in
        let state =
          Safe_sys.command __POS__ state command
        in
        state
      else
        state
    in
    Safe_sys.chdir __POS__ state current
