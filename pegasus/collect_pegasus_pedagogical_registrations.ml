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
  semester: string option;
  inscription_dens: bool option;
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
  semester = None ;
  inscription_dens = None;

}

let rec fetch_name l acc =
  match l with
    | "Student:"::_
    | "Student"::"number:"::_
    | "INE"::"number:"::_
    | "Statut"::_
    | "Tutor:"::_
    | [] -> List.rev acc, l
    | x::_ when String.length x > 0 && (let c =Char.code (String.get x 0) in
  47 < c && c < 58) -> List.rev acc, l

    | h::t -> fetch_name t (h::acc)

let fetch_name l =
  let name, tail = fetch_name l [] in
  let lastname, firstname = Tools.decompose_name name in
  tail, lastname, firstname

let update_student bloc entry state =
    let l = String.split_on_char ' ' bloc in
    let l = List.rev_map (String.split_on_char '\n') (List.rev l) in
    let l = List.flatten l in
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

    let update_bloc' bloc entry state =
        let l = String.split_on_char ' ' bloc in
        let l = List.rev_map (String.split_on_char '\n') (List.rev l) in
        let l = List.flatten l in
        let t, lastname, firstname = fetch_name l in
        let entry = {entry with lastname = Some lastname ; firstname = Some firstname} in
        let rec get tail acc =
            match tail with
              | "Numéro"::"INE"::":"::tail -> Some (List.rev acc), tail
              | t::tail ->
                if String.length t > 1 && String.sub t 0 2 = "20"
                then Some (List.rev acc), t::tail
                else get tail (t::acc)
              | [] -> None, []
        in
        let _statut, t =
            match t with
              | "Statut"::":"::tail -> get tail []
              | tail -> None, tail
        in
        let entry, t = match t with
          | [] -> entry,t
          | n::tail ->{entry with student_number = Some n },tail
        in
        let rec aux t b_opt dpt =
        match t with
          | ":"::tail -> tail,b_opt, dpt
          | "Diplôme"::"de"::"l'ENS-PSL"::dpt::"-"::n::tail
          | "Diplôme"::":"::"Diplôme"::"de"::"l'ENS-PSL"::dpt::"-"::n::tail -> aux tail (Some true) (Some (dpt,n))
          | _::tail -> aux tail b_opt dpt
          | [] -> [], b_opt, dpt
        in
        let tail, b_opt,dpt = aux t None None in
        let _, tutor_lastname, tutor_firstname = fetch_name tail  in
        {entry with tutor_lastname = Some tutor_lastname ; tutor_firstname = Some tutor_firstname ; inscription_dens = b_opt}, state, dpt




let update_year year entry state =
  let l = String.split_on_char ' ' year in
  let l = List.filter (fun x -> x<>"") l in
  match l with "Academic"::"year"::y::_
    | "Année"::"universitaire"::y::_ -> {entry with year = Some y}, state
    | _ -> entry, state


let add unify pos c state =
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
  Public_data.pe_semester = entry.semester;
  Public_data.pe_dens = entry.inscription_dens;
}


let update_diploma diploma entry (state:Remanent_state.t) =
  let code, libelle =
    let l = String.split_on_char ' ' diploma in
    match l with
    | h::t -> h, String.concat " " t
    | [] -> "",""
  in
  let code_helisa, libelle = Some code, Some libelle in
  let state, entry = convert {entry with libelle ; code_helisa} state in
  add
        (fun _ state a _ -> state,a) __POS__
        [entry] state

let update_inscription code_helisa entry state =
  match code_helisa with
    | Some (dpt,n) ->
        let state, dpt =
          match dpt with
            | "Informatique"  -> state, "INF"
            | "Mathématique" -> state, "DMA"
            | "Chimie" -> state, "CHI"
            | _ -> Remanent_state.warn __POS__ (Format.sprintf "Unknown dpt %s " dpt) Exit state, ""
        in
        let state, n =
         match n with
          | "1ère" -> state, 1
          | "2ème" -> state, 2
          | "3ème" -> state, 3
          | _ -> Remanent_state.warn __POS__ (Format.sprintf "Unknwon inscription year %s" n) Exit state, 0
        in
        let code_helisa =
          Some (Format.sprintf "AND%s%i" dpt n) in
        let state, entry = convert {entry with code_helisa} state in
        add (fun _ state a _ -> state,a ) __POS__ [entry] state
    | None -> state

  let update_snd dpt entry state =
    let snd x = Format.sprintf "UNDDSEC-%s" x in
    let state, code =
    match dpt with
    | "Département de physique" -> state, snd "PHYS"
    | "Département de mathématiques et applications" -> state, snd "DMA"
    | "Département de sciences sociales" -> state, snd "DSS"
    | "Département d'informatique" -> state, snd "INFO"
    | "Département de chimie" -> state, snd "CHIMIE"
    | "Département de géosciences" -> state, snd "GEOSCIENCES"
    | "Département de biologie" -> state, snd "IBENS"
    | "Département d'économie" -> state, snd "ECO"
    | "Département des arts" -> state, snd "ART"
    | "Département de philosophie" -> state, snd "PHI"
    | "Département d'études cognitives" -> state, snd "DEC"
    | _ ->
      Remanent_state.warn __POS__ (Format.sprintf "UPDATE_SND' %s %s (%s) @." (Tools.unsome_string entry.firstname) (Tools.unsome_string entry.lastname) dpt) Exit state, "SND-NA"
  in
  let code_helisa, libelle = Some code, Some dpt in
  let state, entry = convert {entry with libelle ; code_helisa } state in
  add
        (fun _ state a _ -> state,a) __POS__
        [entry] state

  let update_diploma' diploma entry (state:Remanent_state.t) =
      let libelle = diploma in
      let state, code =
          match libelle with
          | "Diplôme de M1 suivi à l'ENS-PSL" -> state, "UNDDIPE-M1"
          | "Diplôme de M1 suivi en dehors de l'ENS-PSL" -> state, "UNDDIPH-M1"
          | "Diplôme de L3 suivi à l'ENS-PSL" -> state, "UNDDIPE-L3"
          | "Diplôme de L3 suivi en dehors de l'ENS-PSL" -> state, "UNDDIPH-L3"
          | "Diplôme de M2 à l'ENS-PSL"
          | "Diplôme de M2 suivi à l'ENS-PSL" -> state, "UNDDIPE-M2"
          | "Diplôme de M2 suivi en dehors de l'ENS-PSL" -> state, "UNDDIPH-M2"
          | "Aucun diplôme national" ->  state, "UNDDIPL-NA"
          | "Cours de langue étrangère ou en langue étrangère suivi dans le cadre du diplôme universitaire" -> state, ""
          | _ ->
          Remanent_state.warn __POS__ (Format.sprintf "UPDATE_DIPLOMA' %s %s (%s) @." (Tools.unsome_string entry.firstname) (Tools.unsome_string entry.lastname) libelle) Exit state, "UNDDIPL-NA"
          in
          let code_helisa, libelle = Some code, Some libelle in
          let state, entry = convert {entry with libelle ; code_helisa  } state in
          add
                (fun _ state a _ -> state,a) __POS__
                [entry] state


let get_teachers entry =
  [Special_char.capitalize (Special_char.lowercase (Tools.unsome_string entry.Public_data.pegasus_prof_prenom)),Special_char.uppercase  (Tools.unsome_string entry.Public_data.pegasus_prof_nom)]

let get_teachers entry =
  match get_teachers entry with
  | ["-","-"] -> []
  | x -> x

let update_course course ects entry (state:Remanent_state.t) =
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
    let state, ects =
      try state, float_of_string ects with
      _ -> let l = String.split_on_char '+' ects in
      try state, List.fold_left (fun b a -> (float_of_string a)+.b) 0. l with _ ->
      Remanent_state.warn __POS__ (Format.sprintf "float_of_string %s" ects) Exit state, 0.
    in
    let code_helisa, libelle, ects = Some codehelisa, Some libelle, Some ects in
    let state, pegasus_entry =
        Remanent_state.get_course_in_pegasus ~codehelisa ~year  state
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
                [entry] state
      | Some pegasus_entry ->
        let semester = pegasus_entry.Public_data.pegasus_semester in
        let teachers = get_teachers pegasus_entry in
        let code_gps = pegasus_entry.Public_data.pegasus_codegps in
        let libelle_gps = Some pegasus_entry.Public_data.pegasus_libelle in
        let state, entry = convert {entry with semester ; libelle ; ects ; code_helisa ; code_gps ; teachers ;  libelle_gps } state in
          add
                (fun _ state a _ -> state,a) __POS__
                [entry] state


                let update_course'  semester libelle teacher ects entry state  =
                    let _ = teacher in
                    let semester = Some semester in
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
                    let state, ects =
                      try state, Some (float_of_string ects) with
                      _ -> let l = String.split_on_char '+' ects in
                      try state, Some (List.fold_left (fun b a -> (float_of_string a)+.b) 0. l) with _ ->
                      Remanent_state.warn __POS__ (Format.sprintf "float_of_string %s" ects) Exit state, None
                    in
                    let state, pegasus_entry_opt, libelle =
                        begin
                          let state, pegasus_entry_opt =
                              Remanent_state.get_course_in_pegasus_by_libelle ~libelle ~year ~semester state
                          in
                          match pegasus_entry_opt with
                            | _::_ -> state, Some pegasus_entry_opt, libelle
                            | [] ->
                              let libelle = Tools.simplify_spaces libelle in
                              let state, pegasus_entry_opt =
                                  Remanent_state.get_course_in_pegasus_by_libelle ~libelle ~year ~semester state
                              in
                              begin
                              match pegasus_entry_opt with
                                | _::_ -> state, Some pegasus_entry_opt, libelle
                                | [] -> state, None, libelle
                              end
                        end
                    in
                    match pegasus_entry_opt with
                      | None ->
                        let state =
                          Remanent_state.warn
                          __POS__
                          (Format.sprintf "Pegasus entry is missing %s %s" libelle year)
                          Exit
                          state
                        in
                        let libelle = Some libelle in
                        let code_helisa = None in
                        let state, entry = convert {entry with libelle ; ects ; code_helisa } state in
                          add
                                (fun _ state a _ -> state,a) __POS__
                                [entry] state
                      | Some pegasus_entry ->
                        let state, entries =
                        List.fold_left
                          (fun (state, acc) pegasus_entry ->
                            let teachers = get_teachers pegasus_entry in
                            let code_gps = pegasus_entry.Public_data.pegasus_codegps in
                            let libelle_gps = Some pegasus_entry.Public_data.pegasus_libelle in
                            let code_helisa =
                              Some pegasus_entry.Public_data.pegasus_helisa in
                            let libelle = Some libelle in
                            let state, entry = convert {entry with semester ; libelle ; ects ; code_helisa ; code_gps ; teachers ;  libelle_gps } state in
                            state, entry::acc) (state,[]) pegasus_entry
                        in
                          add
                            (fun _ state a _ -> state,a) __POS__
                            entries state

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
                    let state, csv_opt =
                        Scan_xlss_files.get_csv file state
                    in
                    let csv =
                        match csv_opt with
                          | None -> []
                          | Some l -> l
                    in
                      let rec scan list entry (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let entry, state =
                              begin
                                match h with
                                | ""::"LEARNING AGREEMENT"::_ 
                                | ""::"List of the course":: _ ->
                                    entry, state
                                | ""::""::academic::_ ->
                                      update_year academic entry state
                                | ""::diploma::""::""::""::""::""::_->
                                       entry, update_diploma diploma entry state
                                | ""::course::""::""::ects::_ ->
                                      if course = "" then entry, state else
                                      entry,  update_course course ects entry state
                                | bloc::_ ->  update_student bloc entry state
                                | [] -> entry, state
        end
                          in
                          scan t entry state
                    in
                    let rec scan3 list entry (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let entry, state =
                              begin
                                match h with
                                | "LEARNING AGREEMENT"::_ ->
                                    let state = Remanent_state.warn __POS__ "SCAN3 LEARNING" Exit state in 
                                    entry, state
                                | diploma::""::""::_ ->
                                  let state = Remanent_state.warn __POS__ (Format.sprintf "%s" diploma)  Exit state in 
                                  entry, update_diploma diploma entry state
                                | academic::[] when String.length academic > 2 && String.sub academic 0 3 = "Aca" ->
                                  let state = Remanent_state.warn __POS__ (Format.sprintf "%s" academic)  Exit state in 
                              
                                  update_year academic entry state
                                | course::ects::_ -> 
                                  let state = Remanent_state.warn __POS__ (Format.sprintf "%s %s" course ects)  Exit state in 
                              
                                  if course = "" then entry, state else
                                          entry,  update_course course ects entry state
                                | bloc::_ ->  
                                  let state = Remanent_state.warn __POS__ (Format.sprintf "%s" bloc)  Exit state in 
                                  update_student bloc entry state
                                
                                | [] -> entry, state
        end
                          in
                          scan3 t entry state
                    in
                    let scan2 list state =
                        let split l =
                          let rec aux todo current acc =
                                match todo with
                                  | [] -> List.rev ((List.rev current)::acc)
                                  | h::t ->
                                    begin
                                    match h with ("RÉCAPITULATIF DE L’INSCRIPTION PÉDAGOGIQUE"::_) ->
                                        aux t [h] ((List.rev current)::acc)
                                            | _ -> aux t (h::current) acc
                                    end
                          in
                         aux l [] []
                        in
                        let convert_line line entry state =
                            match line with
                              | sem::""::libelle::""::""::""::""::teacher::""::""::ects::_
                              |
                              sem::libelle::""::""::teacher::""::ects::_  ->   if libelle = "" then state else
                                update_course' sem libelle teacher ects entry state
                              | _ -> state
                        in
                        let convert_recap recapitulatif state =
                            let entry = empty_pegasus_entry in
                            match recapitulatif with
                            | ("RÉCAPITULATIF DE L’INSCRIPTION PÉDAGOGIQUE"::_)::(year::_)::(bloc::_)::_::tail
 ->
                            let entry, state = update_year year entry state in
                            let entry, state, dpt  = update_bloc' bloc entry state in
                            let state = update_inscription dpt entry state in
                            let rec aux_diploma tail state =
                              match tail with [] -> state, []
                                            | (""::""::""::diploma::_)::tail when diploma <> "" ->
                                              let state = update_diploma' diploma entry state in
                                              aux_diploma tail state
                                            | (""::""::diploma::_)::tail when diploma <> "" ->
                                              let state = update_diploma' diploma entry state in
                                              aux_diploma tail state
                                            | (""::diploma::_)::tail
                                            when diploma <> "" ->
                                            let state = update_diploma' diploma entry state in
                                            aux_diploma tail state
                                            | _ -> state, tail
                            in
                            let rec aux_snd tail state =
                              match tail with [] -> state, []
                                            | (""::dpt::_)::tail when dpt <> "" ->
                                            let state = update_snd dpt entry state in
                                            aux_snd tail state
                                            | (""::""::dpt::_)::tail when dpt <> "" ->
                                            let state = update_snd dpt entry state in
                                            aux_snd tail state
                                            | (""::""::""::dpt::_)::tail
                                            when dpt <> "" ->
                                            let state = update_snd dpt entry state in
                                            aux_snd tail state
                                            | _ -> state, tail
                            in
                            let rec aux tail state =
                              match tail with
                                | [] -> state
                                | ("Choix du département secondaire"::_)::tail
                                  ->
                                  let state, tail = aux_snd tail state in
                                  aux tail state
                                | ("Diplôme suivi  pendant l’année universitaire en cours"::_)::tail  ->
                                  let state,tail = aux_diploma tail state in
                                  aux tail state
                                | line::tail ->
                                  let state = convert_line line entry state in
                                  aux tail state
                            in aux tail state
                          | _ -> state
                        in
                        let l = split list in
                        let () = Format.printf "RECAPITULATIFS :%i @." (List.length l) in
                        let state = List.fold_left (fun state recapitulatif ->
                            convert_recap recapitulatif state) state l in
                        state
                    in
                    let state =
                      match csv with
                        (""::"LEARNING AGREEMENT"::_)::_ -> 
                          let state = Remanent_state.warn __POS__ (Format.sprintf "%s SCAN" file) Exit state in 
                          scan csv empty_pegasus_entry state
                        | ("LEARNING AGREEMENT"::_)::_ ->  
                          let state = Remanent_state.warn __POS__ (Format.sprintf "%s SCAN3" file)  Exit state in 
                          scan3 csv empty_pegasus_entry state
                        | ("RÉCAPITULATIF DE L’INSCRIPTION PÉDAGOGIQUE"::_)::_ -> 
                          let state = Remanent_state.warn __POS__ (Format.sprintf "%s SCAN2" file) Exit state in 
                           scan2 csv state
                        | _ -> 
                          let state = Remanent_state.warn __POS__ (Format.sprintf "%s NOT SCANNED" file) Exit state in 
                          let state = match csv with 
                                      | (a::b::_)::_ -> 
                                        Remanent_state.warn __POS__ (Format.sprintf "%s %s" a b) Exit state 
                                        | (a::_)::_ -> 
                                          Remanent_state.warn __POS__ (Format.sprintf "%s" a) Exit state 
                                      
                                      | []::_ -> 
                                        Remanent_state.warn __POS__ (Format.sprintf "EMPTY") Exit state 
                                        | [] -> 
                                          Remanent_state.warn __POS__ (Format.sprintf "FULL EMPTY") Exit state  in 
                          state
                    in
                    let state = Remanent_state.close_event_opt event state in
                     state

                  ) state files_list
            in
            let state = Remanent_state.close_event_opt event state in
            let state = Remanent_state.close_event_opt event_opt state in
            state
