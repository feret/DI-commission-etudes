type kind =
  | Humanities
  | Sciences
  | Ecla
  | Activite
  | Sans_mineure
  | Missing
  | Dummy

let actd = "ACTD"
let info = "INFO"
let dma = "DMA"
let bio = "BIO"
let phys = "PHYS"
let chimie = "CHIMIE"
let phil = "PHIL"
let dec = "DEC"
let ibens = "IBENS"
let arts = "ARTS"
let dsa = "DSA"
let dss = "DSS"
let eco = "ECO"
let lila = "LILA"
let geog = "GEOG"
let hist = "HIST"
let gsc = "GSC"
let ceres = "CERES"
let ecla = "ECLA"
let vetu = "VETU"
let dg = "DG"
let ens = "DENS"
let dri = "DRI"
let xt = "XT"

let sciences = [info;dma;bio;phys;dec;ibens;gsc;chimie]
let humanities = [arts;dsa;eco;lila;phil;hist;dss;geog]
let sans_mineure = [ceres]
let ecla = [ecla;"code3251"]
let activite = [vetu;dg;actd]

let all =
 [
    sciences,Sciences;
    humanities,Humanities;
    ecla,Ecla;
    sans_mineure,Sans_mineure;
    activite,Activite
  ]

let map =
    List.fold_left
        (fun map (list,kind) ->
            List.fold_left
                (fun map elt -> Public_data.StringMap.add elt kind map)
                map list)
        Public_data.StringMap.empty
        all

let translate_main_dpt x =
  match x with
  | Public_data.DI -> info
  | Public_data.ENS -> ens
  | Public_data.PHYS -> phys
  | Public_data.CHIMIE -> chimie
  | Public_data.IBENS -> ibens
  | Public_data.ECO -> eco
  | Public_data.DRI -> dri
  | Public_data.ARTS -> arts
  | Public_data.LILA -> lila
  | Public_data.DMA -> dma

let kind_of_course state code extra =
  if code = "" && extra
  then state, ("", Dummy)
  else
    match String.split_on_char '-' code with
      | t::_ ->
        begin
          match
            Public_data.StringMap.find_opt t map
          with
            | None ->
                begin
                  Remanent_state.warn
                            __POS__
                            (Format.sprintf "Undefined GPS key : (%s) (%s)" code t)
                            Exit
                            state, (t, Missing)
                end
            | Some lbl -> state, (t, lbl)
        end
      | [] ->
          Remanent_state.warn
                  __POS__
                  (Format.sprintf "Ill-formed GPS code %s" code)
                  Exit state, ("", Missing)

let fold_repartition_diplome ~main_dpt ~f_nat ~f_dens state repartition dens =
  let state, dens =
      List.fold_left
          (fun (state, dens) course -> f_nat ~main_dpt (state, dens) course)
          (state,dens)
          repartition.Public_data.diplomes_nationaux
  in
  let state, dens =
      List.fold_left
          (fun (state, dens) course -> f_dens ~main_dpt (state, dens) course)
          (state,dens)
          repartition.Public_data.dens
  in
  state, dens

let f_gen get store ~main_dpt (state,dens) course =
    let code = String.split_on_char '-' course.Public_data.supplement_code in
    let state, code  =
      match code with
        | t::_ ->
          begin
              match String.split_on_char ' ' t with
              | t::_ -> state, t
              | [] ->
                Remanent_state.warn
                    __POS__
                    (Format.sprintf "Ill-formed code: %s" course.Public_data.supplement_code)
                    Exit state, ""
          end
        | [] ->
          Remanent_state.warn
              __POS__
              (Format.sprintf "Ill-formed code: %s" course.Public_data.supplement_code)
              Exit state, ""
    in
    if code = main_dpt then
        let dens_cours_discipline_principale = dens.Public_data.dens_cours_discipline_principale in
        let list = get dens_cours_discipline_principale in
        let dens_cours_discipline_principale = store (course::list) dens_cours_discipline_principale in
        let dens = {dens with Public_data.dens_cours_discipline_principale} in
        state, dens
    else if code = xt then
        let dens_cours_a_trier = dens.Public_data.dens_cours_a_trier in
        let list = get dens_cours_a_trier in
        let dens_cours_a_trier = store (course::list) dens_cours_a_trier in
        let dens = {dens with Public_data.dens_cours_a_trier} in
        state, dens
    else
      let state, (key,kind) = kind_of_course state code course.Public_data.supplement_extra in
      match kind with
      | Ecla ->
      let dens_cours_langue = dens.Public_data.dens_cours_langue in
      let list = dens_cours_langue in
      let dens_cours_langue = course::list in
      let dens = {dens with Public_data.dens_cours_langue} in
      state, dens
      | Activite ->
      let dens_cours_activite = dens.Public_data.dens_cours_activite in
      let list = dens_cours_activite in
      let dens_cours_activite = course::list in
      let dens = {dens with Public_data.dens_cours_activite} in
      state, dens
    | Humanities | Sciences | Sans_mineure ->
      let dens_cours_par_dpt = dens.Public_data.dens_cours_par_dpt in
      begin
        let old =
            match
              Public_data.StringMap.find_opt key dens_cours_par_dpt
            with
              | None -> Public_data.empty_repartition_diplomes
              | Some repartition -> repartition
        in
        let list = get old in
        let repartition = store (course::list) old in
        let dens_cours_par_dpt =
          Public_data.StringMap.add key repartition dens_cours_par_dpt
        in
        state, {dens with Public_data.dens_cours_par_dpt}
      end
      | Missing ->
      let dens_cours_a_trier = dens.Public_data.dens_cours_a_trier in
      let list = get dens_cours_a_trier in
      let dens_cours_a_trier = store (course::list) dens_cours_a_trier in
      let dens = {dens with Public_data.dens_cours_a_trier} in
      state, dens
      | Dummy -> state, dens

let f_nat =
    f_gen
        (fun dens -> dens.Public_data.diplomes_nationaux)
        (fun diplomes_nationaux  dens ->
              {dens with Public_data.diplomes_nationaux})
let f_dens =
    f_gen
        (fun dens -> dens.Public_data.dens)
        (fun dens d_dens -> {d_dens with Public_data.dens})

let split_courses dens state =
  let courses = dens.Public_data.dens_cours_a_trier in
  let dens = {dens with Public_data.dens_cours_a_trier = Public_data.empty_repartition_diplomes} in
  let state, main_dpt = Remanent_state.get_main_dpt state in
  let main_dpt = translate_main_dpt main_dpt in
  fold_repartition_diplome
      ~main_dpt
      ~f_nat ~f_dens
      state courses dens

let split_stages dens state = state, dens
let collect_mineure dens state = state, dens


let dump_repartition ?key repartition (state, total) =
  let i,ects =
    List.fold_left
      (fun (i,ects) course ->
          (i+1,ects+.course.Public_data.supplement_ects))
      (0,0.) repartition.Public_data.diplomes_nationaux
  in
  let i',ects' =
    List.fold_left
      (fun (i,ects) course ->
          (i+1,ects+.course.Public_data.supplement_ects))
      (0,0.) repartition.Public_data.dens
  in
  if i=0 && i'=0 && ects=0. && ects'=0. then state, total
  else
  let () = Remanent_state.open_row state in
  let () =
      match key with None -> ()
                  | Some key ->
      Remanent_state.print_cell key state
  in
  let () = Remanent_state.print_cell (string_of_float ects) state in
  let () = Remanent_state.print_cell (string_of_int i) state in
  let () = Remanent_state.print_cell (string_of_float ects') state in
  let () = Remanent_state.print_cell (string_of_int i') state in
  let () = Remanent_state.close_row state in
  let (j,j',fcts,fcts') = total in
  state, (i+j,i'+j',ects+.fcts,ects'+.fcts')

  let dump_list ?key list (state,total) =
    let i,ects =
      List.fold_left
        (fun (i,ects) course ->
            (i+1,ects+.course.Public_data.supplement_ects))
        (0,0.) list
    in
    if i = 0 && ects = 0. then state, total
    else
      let () = Remanent_state.open_row state in
      let () =
        match key with None -> ()
                    | Some key ->
        Remanent_state.print_cell key state
      in
      let () = Remanent_state.print_cell (string_of_float 0.) state in
      let () = Remanent_state.print_cell (string_of_int 0) state in
      let () = Remanent_state.print_cell (string_of_float ects) state in
      let () = Remanent_state.print_cell (string_of_int i) state in
      let () = Remanent_state.close_row state in
      let (j,j',fcts,fcts') = total in
      state, (j,j'+i,fcts,ects+.fcts')

let add_total l =
    List.fold_left
        (fun (i,i',ects,ects') (j,j',fcts,fcts') ->
                (i+j,i'+j',ects+.fcts,ects'+.fcts'))
        (0,0,0.,0.) l

let label_of_diplome dip =
    match dip.Public_data.diplome_cursus.Public_data.inscription with
      | None ->
        Format.sprintf
          "%s (%s)"
          dip.Public_data.diplome_niveau
          (Public_data.string_of_dpt dip.Public_data.diplome_dpt)
      | Some l -> l

let dump_dens dens state =
    let size = [None;None;None;None;None] in
    let bgcolor = [None;None;None;None;None] in
    let state, main_dpt = Remanent_state.get_main_dpt state in
    let total_init = 0,0,0.,0. in
    (*let () = Remanent_state.log_string state "Discipline principale" in*)
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
        ~title:[["Catégories"];["ECTS diplôme nationaux"];["Nb cours diplôme nationaux"]; ["ECTS DENS"];["Nb cours DENS"]]
        ~title_english:[["Département"];["ECTS diplôme nationaux"];["Nb cours diplôme nationaux"]; ["ECTS DENS"];["Nb cours DENS"]]
        state
    in
    let state, total_principale =
          dump_repartition
            ~key:"Discipline principale" dens.Public_data.dens_cours_discipline_principale (state,total_init)
    in
    let liste = dens.Public_data.dens_cours_par_dpt in
    let state,total_other  =
        Public_data.StringMap.fold
          (fun key  -> dump_repartition ~key:(String.uppercase_ascii key))
          liste (state,total_init)
    in
    let state, total_ecla =
        dump_list
          ~key:"Langues"
          dens.Public_data.dens_cours_langue (state,total_init)
     in
     let state, total_resp =
        dump_list
          ~key:"Responsabilités"
          dens.Public_data.dens_cours_activite (state,total_init) in
     let state, total_to_sort =
        dump_repartition
          ~key:"À trier"
          dens.Public_data.dens_cours_a_trier (state,total_init)
      in
     let total = add_total [total_to_sort; total_resp; total_ecla; total_other; total_principale] in
      let () = Remanent_state.fprintf state "\\hline" in
      let () = Remanent_state.open_row state in
      let (i,i',ects,ects') = total in
      let () = Remanent_state.print_cell "total" state in
      let () = Remanent_state.print_cell (string_of_float ects) state in
      let () = Remanent_state.print_cell (string_of_int i) state in
      let () = Remanent_state.print_cell (string_of_float ects') state in
      let () = Remanent_state.print_cell (string_of_int i') state in
      let () = Remanent_state.close_row state in
      let () = Remanent_state.close_array state in
      let () = Remanent_state.fprintf state "\\end{center}" in
      let () = Remanent_state.fprintf state "\\vfill" in
      let () = Remanent_state.fprintf state "\\begin{center}" in
      let () = Remanent_state.fprintf state "\\begin{minipage}{0.5\\linewidth}" in
      let () = Remanent_state.fprintf state "Nbr inscriptions au DENS : %i (3 sont nécessaires)" dens.Public_data.dens_nb_inscriptions in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "Sortant : %s (doit être sortant)" (*(if dens.Public_data.dens_sortant then "Oui" else "Non")*) "non implémenté" in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "ECTS DENS : %s (72 sont nécessaires)" (string_of_float ects') in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state
                  "ECTS discipline principale : %s (24 sont nécessaires)"
                  (let (_,_,_,ects')=total_principale in (string_of_float ects'))
      in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state
                  "ECTS autres disciplines : %s (24 sont nécessaires)"
                  (let (_,_,_,ects')=total_other in (string_of_float ects'))
      in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state
                  "ECTS langues : %s (24 sont nécessaires%s)"
                  (let (_,_,_,ects')=total_ecla in (string_of_float ects'))
                  (match main_dpt with | Public_data.DI -> " ou un stage à l'étranger" | Public_data.DMA | Public_data.ENS|Public_data.PHYS|Public_data.CHIMIE|Public_data.IBENS|Public_data.ECO|Public_data.DRI|Public_data.ARTS|Public_data.LILA -> "")
      in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "M2 recherche : " in
      let () = (match dens.Public_data.dens_master with
               | [] -> Remanent_state.fprintf state "aucun"
               | l ->
                 List.iter
                   (fun dpl ->
                       Remanent_state.fprintf state "%s ; "
                         (label_of_diplome dpl)) (List.rev l))
      in
      let () = Remanent_state.fprintf state " (M2 recherche en %s obligatoire)" (match main_dpt with Public_data.DMA -> "mathématiques" | Public_data.DI -> "informatique" | (Public_data.ENS|Public_data.PHYS|Public_data.CHIMIE|Public_data.IBENS|Public_data.ECO|Public_data.DRI|Public_data.ARTS|Public_data.LILA)
  -> "informatique") in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "Diplômes autre : " in
      let () = (match dens.Public_data.dens_parcours with
               | [] -> Remanent_state.fprintf state "aucun"
               | l ->
                 List.iter
                   (fun dpl -> Remanent_state.fprintf state "%s ; "
                         (label_of_diplome dpl)) (List.rev l))
      in
      let () = Remanent_state.print_newline state in
      let () =
        match main_dpt with
        | Public_data.DI ->
           begin
               let () = Remanent_state.fprintf state "Cours obligatoires : %i (5 sont nécessaires)" dens.Public_data.dens_nb_mandatory_course in
           let () = Remanent_state.print_newline state in
           let () =
              Remanent_state.fprintf
                  state
                  "Cours de maths/maths info : %i/%i (2 sont nécessaires dont au moins un de maths)"
                  dens.Public_data.dens_nb_math_course
                  (dens.Public_data.dens_nb_math_and_math_info_course - dens.Public_data.dens_nb_math_course)
           in
           let () = Remanent_state.print_newline state in
           ()
           end
           | Public_data.DMA
           | Public_data.ENS | Public_data.PHYS | Public_data.CHIMIE |  Public_data.IBENS|Public_data.ECO|Public_data.DRI|Public_data.ARTS|Public_data.LILA -> ()
      in
      let () = Remanent_state.fprintf state "\\end{minipage}" in
      let () = Remanent_state.fprintf state "\\end{center}" in
      let () = Remanent_state.fprintf state "\\vfill\\mbox{}" in
  state

let repeatable state cours extra =
  match kind_of_course state cours extra with
      | state , (_,(Activite | Dummy | Missing  )) -> state, true
      | state, (_,( Humanities | Ecla
        | Sciences
        | Sans_mineure)) -> state, false
