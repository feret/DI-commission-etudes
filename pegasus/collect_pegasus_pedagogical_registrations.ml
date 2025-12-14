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
  diploma: string option; 
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
  diploma = None; 

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
  let firstname = 
    if lastname = "DANILKIN" && firstname = "Anton" then 
      "Anna"
    else firstname 
  in 
  tail, lastname, firstname

let update_student bloc entry bset state =
    let l = String.split_on_char ' ' bloc in
    let l = List.rev_map (String.split_on_char '\n') (List.rev l) in
    let l = List.flatten l in
    let rec aux l entry =
          match l with
          | "Student:"::t ->
             let t, lastname, firstname = fetch_name t in
             if entry.lastname = Some lastname && entry.firstname = Some firstname 
             then aux t entry 
             else 
                  aux t {entry with lastname = Some lastname; firstname = Some firstname ; diploma = None}
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
    entry, (bset, state)

    let update_bloc' bloc entry state =
        let l = String.split_on_char ' ' bloc in
        let l = List.rev_map (String.split_on_char '\n') (List.rev l) in
        let l = List.flatten l in
        let t, lastname, firstname = fetch_name l in
        let entry = 
          if entry.lastname = Some lastname && entry.firstname = Some firstname 
          then entry 
          else 
          {entry with lastname = Some lastname ; firstname = Some firstname} 
        in
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
          | "Diplôme"::"de"::"l'ENS-PSL"::dpt1::dpt2::dpt3::"-"::n::tail
          | "Diplôme"::":"::"Diplôme"::"de"::"l'ENS-PSL"::dpt1::dpt2::dpt3::"-"::n::tail -> aux tail (Some true) (Some (String.concat " " [dpt1;dpt2;dpt3],n))
          | "Diplôme"::"de"::"l'ENS-PSL"::dpt1::dpt2::"-"::n::tail
          | "Diplôme"::":"::"Diplôme"::"de"::"l'ENS-PSL"::dpt1::dpt2::"-"::n::tail -> aux tail (Some true) (Some (String.concat " " [dpt1;dpt2],n))
         
          | "Diplôme"::"de"::"l'ENS-PSL"::dpt::"-"::n::tail
          | "Diplôme"::":"::"Diplôme"::"de"::"l'ENS-PSL"::dpt::"-"::n::tail -> aux tail (Some true) (Some (dpt,n))
          | _::tail -> aux tail b_opt dpt
          | [] -> [], b_opt, dpt
        in
        let tail, b_opt,dpt = aux t None None in
        let _, tutor_lastname, tutor_firstname = fetch_name tail  in
        {entry with tutor_lastname = Some tutor_lastname ; tutor_firstname = Some tutor_firstname ; inscription_dens = b_opt}, state, dpt




let update_year year entry bset state =
  let l = String.split_on_char ' ' year in
  let l = List.filter (fun x -> x<>"") l in
  match l with "Academic"::"year"::y::_
    | "Année"::"universitaire"::y::_ -> {entry with year = Some y}, (bset, state)
    | _ -> entry, (bset, state)


let add unify pos c (bset,state) =
  List.fold_left (fun (bset,state) elt -> 
    if Public_data.PESET.mem elt bset 
    then 
      bset,state
    else 
      let state = 
        if elt.Public_data.pe_lastname="BOYER" || elt.Public_data.pe_lastname="boyer"
        then 
         Remanent_state.warn __POS__ (Format.sprintf "%s %s %s %s %s %s" elt.Public_data.pe_firstname
        elt.Public_data.pe_lastname elt.Public_data.pe_year elt.Public_data.pe_libelle elt.Public_data.pe_code_helisa
        (match elt.Public_data.pe_code_gps with None -> "none" | Some x -> x)) Exit state 
        else state 
      in 
      Public_data.PESET.add elt bset, 
      Remanent_state.Collector_pedagogical_registrations.add unify pos [elt] state) 
    (bset,state) c 

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
  Public_data.pe_diploma = entry.diploma; 
}


let update_diploma diploma entry (bset,state) =
  let code, libelle =
    let l = String.split_on_char ' ' diploma in
    match l with
    | h::t -> h, String.concat " " t
    | [] -> "",""
  in
  if String.length code > 1 && String.sub code 0 2 = "SN"
    && String.length libelle > 9 && String.sub libelle 0 10 = "- Semestre"
  then bset, state 
  else 
    let code_helisa, libelle = Some code, Some libelle in
    let state, entry = convert {entry with libelle ; code_helisa} state in
    add
        (fun _ state a _ -> state,a) __POS__
        [entry] (bset,state)

let update_inscription code_helisa entry bset state =
  match code_helisa with
    | Some (dpt,n) ->
        let state, dpt =
          match dpt with
            | "Informatique"  -> state, "INF"
            | "Mathématiques et applications" | "Mathématique" -> state, "DMA"
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
        add (fun _ state a _ -> state,a ) __POS__ [entry] (bset,state)
    | None -> bset,state

  let update_snd dpt entry bset state =
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
        [entry] (bset,state)

  let update_diploma' diploma entry bset (state:Remanent_state.t) =
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
            [entry] (bset,state)


let get_teachers entry =
  [Special_char.capitalize (Special_char.lowercase (Tools.unsome_string entry.Public_data.pegasus_prof_prenom)),Special_char.uppercase  (Tools.unsome_string entry.Public_data.pegasus_prof_nom)]

let get_teachers entry =
  match get_teachers entry with
  | ["-","-"] -> []
  | x -> x

let update_course course ects entry bset (state:Remanent_state.t) =
    let codehelisa, libelle =
       let l = String.split_on_char ' ' course in
       match l with
        | h::"-"::t | h::t -> h, String.concat " "  t
        | [] -> "",""
    in
    match codehelisa with 
      | "Semestre" | "Semester" -> bset, state 
      | _ -> 
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
          try 
            state, float_of_string ects 
          with
            | _ -> 
                let l = String.split_on_char '+' ects in
                try 
                  state, List.fold_left (fun b a -> (float_of_string a)+.b) 0. l 
                with _ ->
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
                [entry] (bset,state)
          | Some pegasus_entry ->
            let semester = pegasus_entry.Public_data.pegasus_semester in
            let teachers = get_teachers pegasus_entry in
            let code_gps = pegasus_entry.Public_data.pegasus_codegps in
            let libelle_gps = Some pegasus_entry.Public_data.pegasus_libelle in
            let state, entry = convert {entry with semester ; libelle ; ects ; code_helisa ; code_gps ; teachers ;  libelle_gps } state in
            add
                (fun _ state a _ -> state,a) __POS__
                [entry] (bset,state)


let update_course'  semester libelle teacher ects entry bset state  =
    let _ = teacher in
    let semester = Some semester in
    let libelle = Tools.simplify_libelle libelle in 
    let state, year =
      match entry.year with
        | None ->
              Remanent_state.warn
                __POS__ "Year is missing" Exit state, ""
        | Some y -> state, y
    in
    let state, ects =
        try state, Some (float_of_string ects) 
        with
          | _ -> let l = String.split_on_char '+' ects in
                   try state, Some (List.fold_left (fun b a -> (float_of_string a)+.b) 0. l) 
                   with 
                    | _ ->
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
                                [entry] (bset,state)
                      | Some pegasus_entry ->
                        let state, pegasus_entry = 
                          match pegasus_entry with 
                            | [_] | [] -> state, pegasus_entry
                            | _ -> 
                              let state = Remanent_state.warn __POS__ (Format.sprintf "Several Pegasus entries for the cours label %s (%s) %s" libelle year (match semester with None -> "NA" | Some s -> s)) Exit state in 
                              let state = List.fold_left (fun state a -> Remanent_state.warn __POS__ (Format.sprintf "Domain %s, Libellé %s " (Tools.unsome_string a.Public_data.pegasus_domain) libelle ) Exit state) state pegasus_entry in 
                              let l' = List.filter (fun a -> a.Public_data.pegasus_domain = Some "DENS-ENS") pegasus_entry 
                        in 
                        begin 
                          match l' with [_] -> state, l' | _ -> state, pegasus_entry 
        
                          end 
                        in 
                        let pegasus_entry = 
                            match pegasus_entry with 
                              | a::_ -> [a]
                              | _ -> pegasus_entry 
                        in 
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
                            entries (bset,state)

let event_opt = Some (Profiling.Collect_pegasus_pedagogical_registrations)
let compute_repository = Remanent_state.Collector_pedagogical_registrations.get_repository

let is_academic_year y = 
  let n = String.length y in 
  String.length y > 4 && 
  begin
    let start = String.sub y 0 4 in 
    let finish = String.sub y (n-4) 4 in 
    try 
      int_of_string start + 1 = int_of_string finish 
    with 
      | Failure s -> if s = "int_of_string" then false else raise (Failure s) 
    end

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
                      let rec scan list entry bset (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let entry, (bset, state) =
                              begin
                                let state = 
                                  if entry.lastname = Some "LABBE" then 
                                    List.fold_left 
                                      (fun state a -> Remanent_state.warn __POS__ a Exit state)
                                      state h 
                                  else state 
                                in 
                                match h with
                                | ""::"LEARNING AGREEMENT"::_ -> 
                                     entry, (bset, state)
                                | ""::"List of the courses":: _ ->
                                    entry, (bset, state)
                                | ""::"ANECHINTER - International Exchange"::_ -> 
                                  let entry = {entry with diploma = Some "ANECHINTER"} in 
                                  (*  let entry, (bset, state) = *)
                                     entry, (update_diploma "ANECHINTER - International Exchange" entry (bset,state))
                                | ""::"ANM2INFPRI - Master in Computer science (Second year) - Algorithmic Science"::_ ->
                                  let entry = {entry with diploma = Some "ANM2INFPRI"} in 
                                  (*  let entry, (bset, state) = *)
                                     entry, (update_diploma "ANM2INFPRI - Master in Computer science (Second year) - Algorithmic Science" entry (bset,state))
                                 | ""::"ANM2INFPRI - Master in Computer science (Second year) -"::_-> 
                                   let entry = {entry with diploma = Some "ANM2INFPRI"} in 
                                  (*  let entry, (bset, state) = *)
                                     entry, (update_diploma "ANM2INFPRI - Master in Computer science (Second year) -" entry (bset,state))
                                
                                     | ""::"ANM1INF - Master in Computer science (First year)"::_ -> 
                                      let entry = {entry with diploma = Some "ANM1INF"} in 
                                   (*  let entry, (bset, state) = *)
                                      entry, (update_diploma "ANM1INF - Master in Computer science (First year)" entry (bset,state)) (*in*) 
                                    (*  let course = "UNINF2-045 - Stage long M1 étranger pays non francophone & Long interns" in  
                                      let ects = "30." in
                                      entry,  update_course course ects entry bset state*)
                                | ""::"ANM2INFPRI - Master in Computer science (Second year) - Algorithmic Science "::_  ->                             
                                  let entry = {entry with diploma = Some "ANM2INFPRI"} in 
                                  entry, (update_diploma "ANM2INFPRI - Master in Computer science (Second year) - Algorithmic Science " entry (bset,state))
                                | ""::""::academic::_ ->
                                    update_year academic entry bset state
                                | ""::diploma::""::""::""::_->
                                       entry, update_diploma diploma entry (bset, state)
                                | ""::course::""::""::ects::_ ->
                                      if course = "" 
                                      then entry, (bset, state) 
                                      else
                                        entry,  update_course course ects entry bset state
                                | bloc::_ ->  update_student bloc entry bset state
                                | [] -> entry, (bset,state)
                              end
                          in
                          scan t entry bset state
                    in
                    let rec scan3 list entry bset (state:Remanent_state.t) =
                      match list with
                          | [] -> state
                          | h::t ->
                            let entry, (bset, state) =
                              begin
                                match h with
                                | "LEARNING AGREEMENT"::_ ->
                                    entry, (bset, state)
                                | diploma::""::""::_ ->
                                  entry, update_diploma diploma entry (bset, state)
                                | academic::[] when String.length academic > 2 && String.sub academic 0 3 = "Aca" ->
                                  update_year academic entry bset state
                                | course::ects::_ -> 
                                  if course = "" then entry, (bset, state) else
                                          entry,  update_course course ects entry bset state
                                | bloc::_ ->  
                                  update_student bloc entry bset state
                                
                                | [] -> entry, (bset, state)
        end
                          in
                          scan3 t entry bset state
                    in
                    let scan2 list bset state =
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
                        let convert_line line entry bset state =
                            match line with
                              | sem::""::libelle::""::""::""::""::teacher::""::""::ects::_
                              | sem::libelle::""::""::teacher::""::ects::_  ->   
                                  if libelle = "" 
                                  then bset, state 
                                  else
                                   update_course' sem libelle teacher ects entry bset state
                              | _ -> bset, state
                        in
                        let convert_recap recapitulatif bset (state:Remanent_state.t) =
                            let entry = empty_pegasus_entry in
                            match recapitulatif with
                            | ("RÉCAPITULATIF DE L’INSCRIPTION PÉDAGOGIQUE"::_)::(year::_)::(bloc::_)::tail
 ->
                            let entry, (bset, state) = update_year year entry bset state in
                            let entry, (bset, state), dpt  = update_bloc' bloc entry (bset,state) in
                            let bset, state = update_inscription dpt entry bset state in
                            let rec aux_diploma tail bset state =
                              match tail with [] -> bset, state, []
                                            | (""::""::""::diploma::_)::tail when diploma <> "" ->
                                              let bset, state = update_diploma' diploma entry bset state in
                                              aux_diploma tail bset state
                                            | (""::""::diploma::_)::tail when diploma <> "" ->
                                              let bset, state = update_diploma' diploma entry bset state in
                                              aux_diploma tail bset state
                                            | (""::diploma::_)::tail
                                            when diploma <> "" ->
                                            let bset, state = update_diploma' diploma entry bset state in
                                            aux_diploma tail bset state
                                            | _ -> bset, state, tail
                            in
                            let rec aux_recap_inscriptions tail bset state = 
                              let state = Remanent_state.warn 
                                __POS__ "AUX RECAP STATE %i" Exit state 
                              in 
                              let state = 
                                match tail with [] -> state 
                                | h::_ -> 
                                  List.fold_left 
                                      (fun state l -> 
                                        Remanent_state.warn 
                                          __POS__ (Format.sprintf "AUX RECAP STATE %s" l) 
                                          Exit state) state  h 
                                    
                              in 
                              match tail with [] -> bset, state, []
                              | ("Année"::"Diplôme"::"Statut"::"Tuteur.rice"::"Dpt secondaire"::_)::tail -> 
                                let state = Remanent_state.warn 
                                __POS__ "AUX RECAP STATE ANNEE" Exit state 
                                in 
                                aux_recap_inscriptions tail bset state 
                              | (y::_)::tail when is_academic_year y ->
                                let state = Remanent_state.warn 
                                __POS__ (Format.sprintf "AUX RECAP STATE ANNEE %s" y) Exit state 
                                in  
                                aux_recap_inscriptions tail bset state
                              | _ -> bset, state, tail 
                            in 
                            let rec aux_snd tail bset state =
                              match tail with 
                                  | [] -> bset, state, []
                                  | (""::dpt::_)::tail when dpt <> "" ->
                                        let bset, state = update_snd dpt entry bset state in
                                            aux_snd tail bset state
                                  | (""::""::dpt::_)::tail when dpt <> "" ->
                                        let bset, state = update_snd dpt entry bset state in
                                            aux_snd tail bset state
                                  | (""::""::""::dpt::_)::tail
                                            when dpt <> "" ->
                                         let bset, state = update_snd dpt entry bset state in
                                            aux_snd tail bset state
                                  | _ -> bset, state, tail
                            in
                            let rec aux tail bset state =
                              let state = Remanent_state.warn __POS__ (Format.sprintf "AUX %s" (match tail with [] | []::_ -> "" | (t::_)::_ -> t)) Exit state in 
                              match tail with
                                | [] -> bset, state
                                | ("Période"::_)::tail -> aux tail bset state  
                                | ("Choix du département secondaire"::_)::tail
                                  ->
                                  let bset, state, tail = aux_snd tail bset state in
                                  aux tail bset state
                                | ("RECAPITULATIF INSCRIPTIONS"::_)::tail -> 
                                  let state = Remanent_state.warn __POS__ "RECAP" Exit state in 
                                  let bset, state, tail = aux_recap_inscriptions tail bset state 
                                   in aux tail bset state  
                                | ("Diplôme suivi  pendant l’année universitaire en cours"::_)::tail  ->
                                  let bset, state,tail = aux_diploma tail bset state in
                                  aux tail bset state
                                | line::tail ->
                                  let bset, state = convert_line line entry bset state in
                                  aux tail bset state
                            in aux tail bset state
                          | _ -> bset, state
                        in
                        let l = split list in
                        let () = Format.printf "RECAPITULATIFS :%i @." (List.length l) in
                        let _, state = List.fold_left (fun (bset, state) recapitulatif ->
                            convert_recap recapitulatif bset state) (bset, state) l in
                        state
                    in
                    let state =
                      match csv with
                        (""::"LEARNING AGREEMENT"::_)::_ -> 
                          scan csv empty_pegasus_entry Public_data.PESET.empty state
                        | ("LEARNING AGREEMENT"::_)::_ ->  
                          scan3 csv empty_pegasus_entry Public_data.PESET.empty state
                        | ("RÉCAPITULATIF DE L’INSCRIPTION PÉDAGOGIQUE"::_)::_ -> 
                           scan2 csv Public_data.PESET.empty state
                        | _ -> state 
                    in
                    let state = Remanent_state.close_event_opt event state in
                    state

                  ) state files_list
            in
            let state = Remanent_state.close_event_opt event state in
            let state = Remanent_state.close_event_opt event_opt state in
            state
