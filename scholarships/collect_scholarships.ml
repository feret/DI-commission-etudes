
type scholarship_id =
  {
    lastname: string option;
    firstname: string option;
    promotion: string option;
    organism: string option;
  }


let empty_scholarship =
  {
    lastname = None ;
    firstname = None ;
    promotion = None ;
    organism = None ;
  }

let fun_ignore =
  (fun state _ x -> state, x)
let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.LastName ;
    Public_data.FirstName ;
    Public_data.Promo ;
    Public_data.Organisme_de_Financement ;
  ]

let keywords_of_interest =
  [
    Public_data.LastName ;
    Public_data.FirstName ;
  ]
let asso_list =
  [
    Public_data.LastName,
    (fun state lastname x ->
       state,
       let lastname =
         match lastname with
         | Some x when String.trim x = "" -> None
         | _ -> lastname
       in
       {x with lastname});
    Public_data.FirstName,
    (fun state firstname x ->
       state,
       let firstname =
         match firstname with
         | Some x when String.trim x = "" -> None
         | _ -> firstname
       in
       {x with firstname});
    Public_data.Promo,
    (fun state promotion x ->
       state, {x with promotion});
    Public_data.Organisme_de_Financement,
       (fun state organism x ->
          state, {x with organism});
  ]

let get_scholarships
    ?repository
    ?prefix
    ?file_name
    ?promotion
    state
  =
  let event_opt = Some (Profiling.Collect_scholarships) in
  let state =
    Remanent_state.open_event_opt
      event_opt
      state
  in
  let p promo promo' =
    match promo,promo' with
      Some x, Some y  -> x=y
    | None, _ | _, None -> true
  in
  let at_end_of_array_line
      _header state current_file current_file' output =
    match current_file'.firstname,current_file'.lastname,current_file'.organism with
    | None,None,None -> state, current_file, output
    | Some _, Some _, Some _ ->
      state, current_file, current_file'::output
    | Some x, None, Some y ->
      let note =
        match current_file'.promotion with
        | None -> Format.sprintf " (%s SCHOLARSHIP)" y
        | Some x -> Format.sprintf " (PROMO %s, %s SCHOLARSHIP)" x y
      in
      let msg = Format.sprintf "Last name is missing for %s%s" x note in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | Some x, Some y, None  ->
      let note =
        match current_file'.promotion with
        | None -> ""
        | Some x -> Format.sprintf " (PROMO %s)" x
      in
      let msg =
        Format.sprintf "Organism is missing fos %s %s's scholarship%s"
          x y note
      in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | None, Some x, Some y ->
      let note =
        match current_file'.promotion with
        | None -> Format.sprintf " (%s SCHOLARSHIP)" y
        | Some x -> Format.sprintf " (PROMO %s, %s SCHOLARSHIP)" x y
      in
      let msg = Format.sprintf "First name is missing for %s%s" x note in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | Some x, None, None ->
      let note =
        match current_file'.promotion with
        | None -> ""
        | Some x -> Format.sprintf " (PROMO %s)" x
      in
      let msg = Format.sprintf "Last name and funding organism is missing for %s's scholarship %s" x note in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | None, Some y, None  ->
      let note =
        match current_file'.promotion with
        | None -> ""
        | Some x -> Format.sprintf " (PROMO %s)" x
      in
      let msg =
        Format.sprintf "First name and organism is missing fos %s %s's scholarship" y note
      in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
    | None, None, Some y ->
      let note =
        match current_file'.promotion with
        | None -> Format.sprintf " (%s SCHOLARSHIP)" y
        | Some x -> Format.sprintf " (PROMO %s, %s SCHOLARSHIP)" x y
      in
      let msg = Format.sprintf "First and last name are missing for a scholarship %s%s" y note in
      let state =
        Remanent_state.warn
          __POS__
          msg
          Exit
          state
      in
      state, current_file, output
  in
  let at_end_of_array
      _header state current_file output =
    state, current_file, output
  in
  let at_end_of_file state _current_file output =
    state, output
  in
  let flush state current_file output =
    state, current_file::output
  in
  let state, repository =
    match repository with
    | Some a -> state, a
    | None -> Remanent_state.get_scholarships_list_repository state
  in
  let state, list =
    Scan_csv_files.get_list
      ~keywords_of_interest ~asso_list ~keywords_list
      ~fun_default:fun_ignore
      ~at_end_of_array_line ~at_end_of_array ~at_end_of_file ~flush
      ~init_state:empty_scholarship
      state
      ~repository ?prefix ?file_name
      []
  in
  let state =
    List.fold_left
      (fun state student ->
         if p promotion student.promotion
         then
           match student.firstname, student.lastname, student.organism
           with
           | Some firstname, Some lastname, Some organism ->
             Remanent_state.add_scholarship __POS__
               {Public_data.holder_firstname =
                  String.lowercase_ascii firstname ;
                Public_data.holder_lastname=
                  String.lowercase_ascii lastname;
                Public_data.holder_promotion=
                  student.promotion;
                Public_data.organism =
                  String.lowercase_ascii organism
               }
               state
           | None, None, None -> state
           | Some x, None, Some y ->
             let note =
               match student.promotion with
               | None -> Format.sprintf " (%s SCHOLARSHIP)" y
               | Some x -> Format.sprintf " (PROMO %s, %s SCHOLARSHIP)" x y
             in
             let msg = Format.sprintf "Last name is missing for %s%s" x note in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | Some x, Some y, None  ->
             let note =
               match student.promotion with
               | None -> ""
               | Some x -> Format.sprintf " (PROMO %s)" x
             in
             let msg =
               Format.sprintf "Organism is missing fos %s %s's scholarship%s" x y note
             in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, Some x, Some y ->
             let note =
               match student.promotion with
               | None -> Format.sprintf " (%s SCHOLARSHIP)" y
               | Some x -> Format.sprintf " (PROMO %s, %s SCHOLARSHIP)" x y
             in
             let msg = Format.sprintf "First name is missing for %s%s" x note in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | Some x, None, None ->
             let note =
               match student.promotion with
               | None -> ""
               | Some x -> Format.sprintf " (PROMO %s)" x
             in
             let msg = Format.sprintf "Last name and funding organism is missing for %s's scholarship %s" x note in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, Some y, None  ->
             let note =
               match student.promotion with
               | None -> ""
               | Some x -> Format.sprintf " (PROMO %s)" x
             in
             let msg =
               Format.sprintf "First name and organism is missing fos %s %s's scholarship" y note
             in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
           | None, None, Some y ->
             let note =
               match student.promotion with
               | None -> Format.sprintf " (%s SCHOLARSHIP)" y
               | Some x -> Format.sprintf " (PROMO %s, %s SCHOLARSHIP)" x y
             in
             let msg = Format.sprintf "First and last name are missing for a scholarship %s%s" y note in
             Remanent_state.warn
               __POS__
               msg
               Exit
               state
         else
           state)
      state list
  in
  let state =
    Remanent_state.close_event_opt
      event_opt
      state
  in
  state
