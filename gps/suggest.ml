let collect dens state = 
    let size = [None;None;None;None;None;None] in
    let bgcolor = [None;None;None;None;None;None] in
    let state, _main_dpt = Remanent_state.get_main_dpt state in
    let _total_init = 0,0,0.,0. in
    let () = Remanent_state.fprintf state "\\renewcommand{\\row}[5]{#1&#2&#3&#4&#5\\cr}" in
    let () = Remanent_state.fprintf state "\\renewcommand{\\innerline}{}" in
    let () = Remanent_state.fprintf state "\\vfill" in
    let () = Remanent_state.fprintf state "\\begin{center}" in
    let state =
      Remanent_state.open_array
        __POS__
        ~bgcolor
        ~size
        ~with_lines:true
        ~title:[["Code"];["Code"];["Note"];["ECTS"]; ["DIPLOMA"];["DPT"]]
        ~title_english:[["Code"];["Code"];["Note"];["ECTS"]; ["DIPLOMA"];["DPT"]]
        state
    in
    let state = 
      Reglements_pedagogiques_tools.CourseDMap.fold  
      (fun _ c state -> 
        let () = Remanent_state.open_row state in
        let () = Remanent_state.print_cell (match c.Public_data.supplement_code_gps with None -> "" | Some a -> a) state in 
        let () = Remanent_state.print_cell (match c.Public_data.supplement_code_helisa with None -> "" | Some a -> a) state in 
        let state, note = Notes.to_string __POS__ state c.Public_data.supplement_note in 
        let () = Remanent_state.print_cell note state in 
        let () = Remanent_state.print_cell (string_of_float c.Public_data.supplement_ects) state in 
        let () = Remanent_state.print_cell
          (match 
            c.Public_data.supplement_diploma_level 
          with 
            | Public_data.L3 -> "L3" 
            | Public_data.M1 -> "M1" 
            | Public_data.M2 -> "M2" 
            | Public_data.DENS -> "DENS" 
            | Public_data.Other -> "Other") state in 
        let () = Remanent_state.print_cell (Public_data.string_of_dpt_opt c.Public_data.supplement_diploma_dpt) state in 
        let () = Remanent_state.close_row state in 
        state 
         
        ) dens state 
  in 
  let () = Remanent_state.close_array state in 
    state, dens 
    