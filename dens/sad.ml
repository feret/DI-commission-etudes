let times = ["double";"triple";"quadruple";"quintuple"]
let ordre = ["première";"seconde";"troisième";"quatrième";"cinquième"]
let order = ["first";"second";"third";"fourth";"fifth"]

let string_of_float x = if x=0. then "" else string_of_float x

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

let width_gps_code = 10.
let width_discipline = 10.
let width_intitule = 20.
let width_ects = 2.

let undef = "une"

let pos_order_1 = ordre,order
let next (a,b) = List.tl a,List.tl b

let lift_dens dens =
    let diplomes_nationaux = [] in
    {Public_data.dens; Public_data.diplomes_nationaux}

let compute_size l =
  let sum =
      List.fold_left
        (fun total a -> total+.a)
        0. l
  in
  let size =
      List.rev_map
        (fun a -> Some (a/.(sum*.1.12)))
        (List.rev l)
  in size

let size4 = compute_size [width_gps_code;width_discipline;width_intitule;width_ects]
let size3 = compute_size [width_gps_code;width_discipline;width_intitule]


let dump_course_gen label is_empty fold iter acc state =
    if is_empty acc  then state
    else
    let () = Remanent_state.fprintf state "\\textbf{%s}" label in
    let () = Remanent_state.print_newline state in
    let ects =
          fold
            (fun cours ects -> ects+.cours.Public_data.supplement_ects) acc 0.
    in
    let () = Remanent_state.fprintf state "Nombre d'ECTS~: %s" (string_of_float  ects) in
    let () = Remanent_state.print_newline state in
    let size = size4 in
    let bgcolor = [None;None;None;None] in
    let state = Remanent_state.open_array
                ~bgcolor
                ~size
                ~with_lines:true
                ~title:[["Code"];["Discipline"];["Intitulé"]; ["ECTS"]]
                ~title_english:[["Code"];["Discipline"];["Name"]; ["ECTS"]]
                __POS__
                state
    in
    let () =
      iter
        (fun elt ->
          let () = Remanent_state.open_row state in
          let () = Remanent_state.print_cell (elt.Public_data.supplement_code) state in
          let () = Remanent_state.print_cell (elt.Public_data.supplement_discipline) state in
          let () = Remanent_state.print_cell (elt.Public_data.supplement_intitule) state in
          let () = Remanent_state.print_cell (string_of_float elt.Public_data.supplement_ects) state in
          let () = Remanent_state.close_row state in
          ())
        acc
    in
    let () = Remanent_state.close_array state in
    let () = Remanent_state.fprintf state "\\mbox{}\\bigskip" in
    let () = Remanent_state.print_newline state in
    state

    let fold_left_tild f a b = List.fold_left (fun a b -> f b a) b a

    let dump_course_list label list state =
        let list = list.Public_data.dens in
        dump_course_gen label (fun l -> l = []) fold_left_tild List.iter list state

    let dump_course_list_autre label list map state =
        let list = list.Public_data.dens in
        dump_course_gen label
          (fun (list,map) -> list=[] && Public_data.StringMap.is_empty map)
          (fun f (list,map) acc ->
            fold_left_tild f list
              (Public_data.StringMap.fold (fun _  l ->
                    fold_left_tild f l.Public_data.dens) map acc ))
          (fun f (list,map) ->
            Public_data.StringMap.iter (fun _ l -> List.iter f l.Public_data.dens) map;
            List.iter f list)
          (list,map)
          state

let dump_repartition_diplomes label list state =
    dump_course_list label list state



let dump_min_maj label map state pos =
    if Public_data.StringMap.cardinal map = 1
    then
      Public_data.StringMap.fold
        (fun key list state ->
            let label = Format.sprintf  "Enseignements validés dans le cadre d'%s %s en %s" undef label key in
            dump_repartition_diplomes label list state)
        map state
    else
      fst (Public_data.StringMap.fold
        (fun key list (state,pos) ->
          let label = Format.sprintf  "Enseignements validés dans le cadre d'une %s %s en %s" (List.hd (fst pos)) label key in
          dump_repartition_diplomes label list state,next pos)
      map (state,pos))

let dump_activite_list label list state =
    if list = [] then state
    else
    let () = Remanent_state.fprintf state "\\textbf{%s}" label in
    let () = Remanent_state.print_newline state in
    let ects =
        List.fold_left
          (fun ects cours -> ects+.cours.Public_data.activite_ects) 0. list
    in
    let state,size,bgcolor,title,title_english,ects_ =
        if ects = 0.
        then state,size3,[None;None;None],[["Code"];["Activité"];["Intitulé"]],[["Code"];["Activity"];["Name"]],false
        else
        let () = Remanent_state.fprintf state "\\textbf{Unités d’enseignement étudiées et nombre d'ECTS}" in
        let () = Remanent_state.print_newline state in
        let () = Remanent_state.fprintf state "\\textbf{Nombre d'ECTS~: %s}" (string_of_float ects) in
        let () = Remanent_state.print_newline state in
        state,size4,[None;None;None;None],[["Code"];["Activité"];["Intitulé"]; ["ECTS"]],[["Code"];["Activity"];["Name"]; ["ECTS"]],true
    in
    let state = Remanent_state.open_array
                ~bgcolor
                ~size
                ~with_lines:true
                ~title
                ~title_english
                __POS__
                state
    in
    let () =
        List.iter
            (fun elt ->
              let () = Remanent_state.open_row state in
              let () = Remanent_state.print_cell
                          (elt.Public_data.activite_code) state in
              let () = Remanent_state.print_cell (Tools.unsome_string  elt.Public_data.activite_activite_fr) state in
              let () = Remanent_state.print_cell (Tools.unsome_string elt.Public_data.activite_intitule_fr) state in
              let () = if ects_ then
                  Remanent_state.print_cell (Format.sprintf "%s" (string_of_float elt.Public_data.activite_ects)) state in
              let () = Remanent_state.close_row state in
              ())
            list
        in
      let () = Remanent_state.close_array state in
      let () = Remanent_state.fprintf state "\\mbox{}\\bigskip" in
      let () = Remanent_state.print_newline state in
      state

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
      Remanent_state.fprintf state "PROMO %s\\bigskip" dens.Public_data.dens_promotion
    in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\textbf{Enseignements complémentaires suivis et validés dans le cadre du Diplôme de l'ENS, et ECTS obtenus}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "Nombre d'ECTS~: %s\\bigskip" (string_of_float  dens.Public_data.dens_total_ects) in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.print_newline state in
    let state =
        dump_course_list
            "Enseignements disciplinaires (cours et stages de recherche dans la spécialité principale)" dens.Public_data.dens_cours_discipline_principale state
    in
    let state = dump_min_maj "mineure" dens.Public_data.dens_cours_mineure state pos_order_1 in
    let state = dump_min_maj "majeure" dens.Public_data.dens_cours_majeure state (maj dens.Public_data.dens_cours_majeure) in
    let state =
        dump_course_list_autre
            "Enseignements hors discipline principale (hors cours de langues)"
            dens.Public_data.dens_cours_hors_disciplines_principale
            dens.Public_data.dens_cours_par_dpt state
    in
    let state =
        dump_course_list
            "Cours de langues étrangères et certifications en langues"
            (lift_dens dens.Public_data.dens_cours_langue) state
    in
    let state = dump_activite_list "Expérience de recherche (collective pour les lettres, de laboratoire pour les sciences)" dens.Public_data.dens_activite_recherche state in
    let state = dump_activite_list "Expérience internationale (stages académiques ou non-académiques à l'étranger)" dens.Public_data.dens_activite_internationale state in
    let state =
        dump_activite_list
          "Expérience d'ouverture hors les murs (stages non-académiques uniquement, en France ou à l'étranger: stages en administration, entreprise, lycée, ONG, etc.)" dens.Public_data.dens_activite_ouverture state
    in
    let state = dump_activite_list "Autre" dens.Public_data.dens_activite_autre state in
    let state = dump_course_list "Autres (vie universitaire, initiatives citoyennes, sport, etc.)" (lift_dens dens.Public_data.dens_cours_activite) state
    in state

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
    && match dens.Public_data.dens_ok with
        | Some true -> true
        | Some false | None -> false
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
       let output = (rep,file) in
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
             {Loggers.orientation = Loggers.Normal ;
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
         let state =
              Latex_engine.latex_opt_to_pdf
                    ~times:2 state ~input:(Some output)
         in
             match
               Remanent_state.get_diplomation_rep ~firstname ~lastname
                state
            with
          | state, None -> state
          | state, Some output_rep ->
            Remanent_state.push_copy
           ~input_rep:rep
           ~file_name:(snd output)
           ~output_rep
           state
      end
    else
      state

let dump_sad ~repository?firstname ?lastname ?language ?bilingual state =
  let state, dens_list = Remanent_state.get_dens state in
    List.fold_right
      (dump_one_sad ~repository ?language ?bilingual ?firstname ?lastname)
      dens_list state
