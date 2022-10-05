type kind = Humanities | Sciences | Ecla | Activite | Sans_mineure | Missing

let info = "INFO"
let dma = "DMA"
let bio = "BIO"
let phys = "PHYS"
let dec = "DEC"
let ibens = "IBENS"
let arts = "ARTS"
let dsa = "DSA"
let eco = "ECO"
let lila = "LILA"
let ceres = "CERES"
let ecla = "ECLA"
let vetu = "VETU"
let dg = "DG"
let ens = "DENS"
let dri = "DRI"

let sciences = [info;dma;bio;phys;dec;ibens]
let humanities = [arts;dsa;eco;lila]
let sans_mineure = [ceres]
let ecla = [ecla]
let activite = [vetu;dg]

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
  | Public_data.IBENS -> ibens
  | Public_data.ECO -> eco
  | Public_data.DRI -> dri
  | Public_data.ARTS -> arts
  | Public_data.LILA -> lila
  | Public_data.DMA -> dma

let kind_of_course state code  =
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
        | t::_ -> state, t
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
    else
      let state, (_,kind) = kind_of_course state code in
      match kind with
      | Ecla -> state, dens
      | Humanities | Sciences | Sans_mineure -> state, dens
      | Activite -> state, dens
      | Missing -> state, dens

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

let size = [None;None;None;None;None]
let bgcolor = [None;None;None;None;None]

let dump_dens dens state =
    let state =
      Remanent_state.open_array
        __POS__
        ~bgcolor
        ~size
        ~with_lines:true
        ~title:[["Département"];["ECTS diplôme nationaux"];["Nb cours diplôme nationaux"]; ["ECTS DENS"];["Nb cours DENS"]]
        ~title_english:[["Département"];["ECTS diplôme nationaux"];["Nb cours diplôme nationaux"]; ["ECTS DENS"];["Nb cours DENS"]]
        state
    in
    let liste = dens.Public_data.dens_cours_par_dpt in
    let state =
        Public_data.StringMap.fold
          (fun key repartition state ->
              let () = Remanent_state.print_cell key state in
              let i,ects =
                List.fold_left
                    (fun (i,ects) course ->
                        (i+1,ects+.course.Public_data.supplement_ects))
                    (0,0.) repartition.Public_data.diplomes_nationaux
              in
              let () = Remanent_state.print_cell (string_of_int i) state in
              let () = Remanent_state.print_cell (string_of_float ects) state in
              let () = Remanent_state.close_row state in
              state)
          liste state
    in
   let () = Remanent_state.close_array state in
    state
