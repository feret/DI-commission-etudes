let times = ["double";"triple";"quadruple";"quintuple"]
let ordre = ["première";"seconde";"troisième";"quatrième";"cinquième"]
let order = ["first";"second";"third";"fourth";"fifth"]

let maj map =
  let n = Public_data.StringMap.cardinal map in
  let rec aux n l =
    if n<=1 then List.hd l
    else aux (n-1) (List.tl l)
  in
  let elt = aux n times in
  let rec aux2 k l =
      match k with 0 -> l | n -> aux2 (n-1) (elt::l)
  in
  let l = aux2 10 [] in l,l


let undef = "une"

let pos_order_1 = ordre,order
let next (a,b) = List.tl a,List.tl b

let lift_dens dens =
    let diplomes_nationaux = [] in
    {Public_data.dens; Public_data.diplomes_nationaux}

let dump_course_list label list state =
    let list = list.Public_data.dens in
    if list = [] then state
    else
    let () = Remanent_state.fprintf state "%s" label in
    let () = Remanent_state.print_newline state in
    let () =
      List.iter
        (fun elt ->
          let () = Remanent_state.fprintf state "%s" elt.Public_data.supplement_intitule in
          Remanent_state.print_newline state)
        list
    in state

let dump_repartition_diplomes label list state =
    dump_course_list label list state

let dump_min_maj label map state pos =
    if Public_data.StringMap.cardinal map = 1
    then
      Public_data.StringMap.fold
        (fun key list state ->
            let label = Format.sprintf  "%s %s %s" undef label key in
            let () = Remanent_state.print_newline state in
            dump_repartition_diplomes label list state)
        map state
    else
      fst (Public_data.StringMap.fold
        (fun key list (state,pos) ->
          let label = Format.sprintf  "%s %s %s" (List.hd (fst pos)) label key in
          let () = Remanent_state.print_newline state in
          dump_repartition_diplomes label list state,next pos)
      map (state,pos))

let dump_activite_list label list state =
    if list = [] then state
    else
    let () = Remanent_state.fprintf state "%s" label in
    let () = Remanent_state.print_newline state in
    let () =
        List.iter
            (fun elt ->
              let () = Remanent_state.fprintf state "%s" elt.Public_data.activite_intitule in
              Remanent_state.print_newline state)
            list
        in state

let prompt_sad dens state =
    let lastname =
        Special_char.uppercase dens.Public_data.dens_lastname
    in
    let firstname =
      Special_char.capitalize dens.Public_data.dens_firstname
    in
    let () =
      Remanent_state.fprintf
        state "%s %s" lastname firstname
    in
    let () =
      Remanent_state.print_newline state
    in
    let () =
      Remanent_state.fprintf state "PROMO %s" dens.Public_data.dens_promotion
    in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "Total ECTS: %f" dens.Public_data.dens_total_ects in
    let () = Remanent_state.print_newline state in
    let state = dump_course_list "Renforcement disciplinaire" dens.Public_data.dens_cours_discipline_principale state in
    let state = dump_min_maj "mineure" dens.Public_data.dens_cours_mineure state pos_order_1 in
    let state = dump_min_maj "majeure" dens.Public_data.dens_cours_majeure state (maj dens.Public_data.dens_cours_majeure) in
    let state = dump_course_list "Autres disciplines" dens.Public_data.dens_cours_hors_disciplines_principale state in
    let state = dump_course_list "Langues" (lift_dens dens.Public_data.dens_cours_langue) state in
    let state = dump_activite_list "Recherche" dens.Public_data.dens_activite_recherche state in
    let state = dump_activite_list "Internationale" dens.Public_data.dens_activite_internationale state in
    let state = dump_activite_list "Ouverture" dens.Public_data.dens_activite_ouverture state in
    let state = dump_activite_list "Autre" dens.Public_data.dens_activite_autre state in
    state

(*dens_main_dpt : main_dpt ;
    dens_firstname : string ;
    dens_lastname : string ;
    dens_promotion : string ;
    dens_total_ects : float ;
    dens_current_year_ects : float ;
    dens_sortant: bool option;
    dens_derogation: bool;
    dens_total_potential_ects : float ;
    dens_current_year_potential_ects : float ;
    dens_nb_inscriptions : int ;
    dens_nb_mandatory_course : int ;
    dens_nb_math_course : int ;
    dens_nb_math_and_math_info_course : int ;
    dens_master : diplome_national list;
    dens_parcours: diplome_national list ;
    dens_cours_a_trier: cours_supplement list repartition_diplomes ;
    dens_cours_discipline_principale: cours_supplement list repartition_diplomes ;
    dens_cours_hors_disciplines_principale: cours_supplement list repartition_diplomes;
    dens_cours_par_dpt: cours_supplement list repartition_diplomes StringMap.t;
    dens_cours_activite: cours_supplement list;
    dens_cours_langue: cours_supplement list;
    dens_cours_mineure: cours_supplement list repartition_diplomes StringMap.t;
    dens_cours_majeure: cours_supplement list repartition_diplomes StringMap.t;
    dens_activite_a_trier: experience_supplement list;
    dens_activite_recherche: experience_supplement list;
    dens_activite_internationale: experience_supplement list;
    dens_activite_ouverture: experience_supplement list;
    dens_activite_autre: experience_supplement list;
    dens_diplomation_year: string;
    dens_ok : bool option ;*)



let check a b =
  match a with
    | None -> true
    | Some a -> a=b

let dump_one_sad ~repository ?firstname ?lastname ?language ?bilingual dens state =
    if check firstname dens.Public_data.dens_firstname
    && check lastname dens.Public_data.dens_lastname
    then
      begin
      let lastname =
        Special_char.uppercase dens.Public_data.dens_lastname
      in
      let firstname =
        Special_char.capitalize dens.Public_data.dens_firstname
      in
       let output = (repository, Format.sprintf "DENS_%s_%s.tex" lastname firstname) in
       let state, language =
        Tools.get_option
          state
          Remanent_state.get_language
          language
       in
       let state, bilinguage =
        Tools.get_option
          state
          Remanent_state.get_is_bilingual
          bilingual
       in
       let state, rep =
         Safe_sys.rec_mk_when_necessary __POS__
           state (fst output)
       in
       let file = snd output in
       let file =
         if rep = ""
         then
           file
         else
           Printf.sprintf "%s/%s" rep file
       in
       let state, output_channel_opt =
         try
           state, Some (open_out file)
         with exn ->
           let msg = Printexc.to_string exn in
           let () =
             Format.printf
               "Cannot open file %s (%s)@."
               file
               msg
           in
           Remanent_state.warn
             __POS__
             (Format.sprintf "Cannot open file %s (%s)"  file msg)
             Exit
             state ,
           None
       in
       match output_channel_opt with
       | None -> state
       | Some out ->
         let mode = Loggers.Latex
             {Loggers.orientation = Loggers.Landscape ;
              Loggers.language =
                (match language with
                | Public_data.French -> Loggers.French
                | Public_data.English -> Loggers.English );
              Loggers.bilinguage =
                bilinguage
             }
         in
         let logger = Loggers.open_logger_from_channel ~mode out in
         let old_logger = Remanent_state.save_std_logger state in
         let state = Remanent_state.set_std_logger state logger in
         let state = prompt_sad dens state in
         let state = Remanent_state.close_logger state in
         let state = Remanent_state.restore_std_logger state old_logger in
         state
      end
    else
      state

let dump_sad ~repository?firstname ?lastname ?language ?bilingual state =
  let state, dens_list = Remanent_state.get_dens state in
    List.fold_right
      (dump_one_sad ~repository ?language ?bilingual ?firstname ?lastname)
      dens_list state