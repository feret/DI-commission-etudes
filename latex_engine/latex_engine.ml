let latex_to_pdf ?save_rep ?rev ?times:(times=1) ~input state =
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
    let rec aux k state =
      if k<1 then state
      else
        aux (k-1)
          (Safe_sys.command __POS__ state command)
    in
    let state = aux times state in
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
        state
      else
        state
    in
    let state =
      match save_rep with
      | None -> state
      | Some save_rep_list ->
        List.fold_left
          (fun state save_rep ->
             let state,save_rep  =
               Safe_sys.rec_mk_when_necessary __POS__ state save_rep
             in
             Safe_sys.cp __POS__ state
               (Format.sprintf "%s.pdf" basename) save_rep)
          state save_rep_list 
    in
    Safe_sys.chdir __POS__ state current

let latex_opt_to_pdf ?save_rep ?rev ?times:(times=1) ~input state =
  match input with
  | None -> state
  | Some input -> latex_to_pdf ?save_rep ?rev ~times ~input state

let concat_pdf ~pattern ~output state =
  let state, output_rep =
    Safe_sys.rec_mk_when_necessary __POS__ state (fst output)
  in
  let output =
    match output_rep with
    | "" -> snd output
    | _ -> Format.sprintf "%s/%s" output_rep (snd output)
  in
  let command =
    Format.sprintf
      "pdftk %s cat output %s" pattern output
  in
  let state = Safe_sys.command __POS__ state command in
  state
