type t =
      Public_data.pedagogical_registration_suggestion
        Public_data.FirstNameMap.t Public_data.LastNameMap.t

let empty =
      Public_data.LastNameMap.empty

let get_pedagogical_registration_suggestions ~firstname ~lastname  dens_candidates =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in
  let l =
  match
    Public_data.LastNameMap.find_opt
      lastname
      dens_candidates
  with
  | None -> None 
  | Some a ->
    match
      Public_data.FirstNameMap.find_opt
        firstname
        a
    with
    | None -> None 
    | Some a -> Some a
  in
  l

  let dump_elt ~print_cell  = 
     (fun state (c,(_,s),(_,s')) -> 
        let libelle = match c.Public_data.supplement_intitule_biling with None -> "" | Some a -> a in
        let () = print_cell (match c.Public_data.supplement_code_gps with None -> "" | Some a -> a) state in 
        let () = print_cell (match c.Public_data.supplement_code_helisa with None -> "" | Some a -> a) state in 
        let () = print_cell libelle state in 
        let note = match c.Public_data.supplement_note_string with None -> "" | Some a -> a in 
        let () = print_cell note state in 
        let () = print_cell (string_of_float c.Public_data.supplement_ects) state in   
         let () = print_cell (match s with None -> "" | Some a -> a) state in 
         let () = print_cell (match s' with None -> "" | Some a -> a) state in 

        ()) 

let dump 
    ~show_missing_entries ~(fprintf:'state -> string->unit) ~print_newline 
    ~print_cell ~breakpage 
    ~bilingual_string ~open_array ~open_row ~close_row ~close_array 
    state ((missing_entries,by_year):Public_data.pedagogical_registration_suggestion) =   
    if missing_entries = [] && by_year = Public_data.YearMap.empty then state else  
    let size =    [None;None;None;None;None;None;None] in
    let bgcolor = [None;None;None;None;None;None;None] in
    let state, show_missing_entries = show_missing_entries state in 
    let state, something = 
      if show_missing_entries then 
      List.fold_left 
      (fun (state,_something) ((_,s), k,l) -> 
            let s = match s with None -> "" | Some a -> a in 
            if k = List.length l then 
              if k = 1 then 
               let () = fprintf state (Format.sprintf "The following %i course " (List.length l)) in 
               let () = List.iter (fun (elt:string) -> fprintf state (Format.sprintf "%s " elt)) l in 
               let () = fprintf state(Format.sprintf "is missing for diploma %s" s) in       
               let () = print_newline state in 
               state, true 
            else  
               let () = fprintf state (Format.sprintf "The following %i courses " (List.length l))  in 
               let () = List.iter (fun elt -> fprintf state (Format.sprintf "%s, " elt)) l in 
               let () = fprintf state (Format.sprintf "are missing for diploma %s" s) in       
               let () = print_newline state in 
               state, true 
            else          
            let () = if k = 1 then fprintf state (Format.sprintf "It misses %i over %i course among " k (List.length l)) 
            else 
              fprintf state (Format.sprintf "It misses %i over %i courses among " k (List.length l))
           in 
            let () = List.iter (fun elt -> fprintf state (Format.sprintf "%s, " elt)) l in 
            let () = fprintf state (Format.sprintf " for diploma %s" s) in         
            let () = print_newline state in 
            state, true) 
          (state, false) missing_entries 
       else (state, false) 
    in 
    let () = fprintf state "\\renewcommand{\\row}[7]{#1&#2&#3&#4&#5&#6&#7\\cr}" in
    let () = fprintf state "\\renewcommand{\\innerline}{}" in
    let () = fprintf state "\\vfill" in
    let state, something = 
      Public_data.YearMap.fold 
        (fun year t (state, _something) -> 
          let year_ext = 
            try 
              let year_int = int_of_string year in 
              Format.sprintf "%i - %i" year_int (year_int + 1) 
            with _ -> year 
          in
          let s_fr = Format.sprintf "Année académique %s" year_ext in 
          let s_en = Format.sprintf "Academic year %s" year_ext in 
          let state, s_bi = 
          bilingual_string ?english:(Some s_en) ~french:s_fr state in 
          let () = fprintf state (Format.sprintf "%s" s_bi) in 
          let () = fprintf state "\\begin{center}" in
    let state =
      open_array
        __POS__
        ~bgcolor
        ~size
        ~with_lines:true
        ~title:[["Code GPS"];["Code HELISA"];["Cours"];["Note"];["ECTS"]; ["DIPLOME (avant)"];["DIPLOME (après)"]]
        ~title_english:[["GPS Code"];["HELISA Code"];["Course"];["Grade"];["ECTS"];["DIPLOMA (before)"] ;["DIPLOMA (after)"]]
        state
    in
    let state = Public_data.StringMap.fold      
         (fun _k c state -> 
        let () = open_row state in
        let () = dump_elt ~print_cell state c in 
        let () = close_row state in 
        state 
        ) t state 
    in
    let () = close_array state in 
    state,true) by_year (state, something) 
  in 
  let () = if something then 
  let () = fprintf state "\\vfill" in
  let () = breakpage state in () 
  in state 

  
let add_pedagogical_registration_suggestions
    ~firstname ~lastname 
    unify pos state
    dens_candidate dens_candidates =
  let _ = unify, pos in
  
  (*let () = Format.printf "%s %s (%s) (ADD PEGAGUS)" firstname lastname year in*)
    let dens_candidates =
    let old_lastname =
      match
        Public_data.LastNameMap.find_opt
          lastname
          dens_candidates
      with
      | Some map -> map
      | None -> Public_data.FirstNameMap.empty
    in
    let per_name =
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname
            dens_candidate
           old_lastname)
        dens_candidates
    in
    per_name
  in
  state, dens_candidates

let fold ~fold_name ~fold_year ~fold_missing ~(fold_entry:((Public_data.cours_supplement *
          ((Public_data.diploma_level option * Public_data.main_dpt option) * string option) *
          ((Public_data.diploma_level option * Public_data.main_dpt option) * string option))
         Public_data.StringMap.t  -> 'a -> 'a)) (t:t) state = 
  Public_data.LastNameMap.fold 
  (fun lastname map state -> 
    (Public_data.FirstNameMap.fold 
      (fun firstname (missing,map) state -> 
          let state = fold_name ~firstname ~lastname state in 
          let state = Public_data.YearMap.fold 
            (fun year t state-> 
              let state = fold_year year state in 
              let state = fold_entry t state in 
              state) 
          map state in 
          List.fold_left (fun state a -> fold_missing a state) state missing 
          ) 
      map state
    )
  ) 
  t state 