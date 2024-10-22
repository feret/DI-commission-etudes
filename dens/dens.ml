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
let chimie = "CHIM"
let gsc = "GSC"
let phil = "PHIL"
let dec = "DEC"
let arts = "ARTS"
let dsa = "DSA"
let dss = "DSS"
let eco = "ECO"
let lila = "LILA"
let geog = "GEOG"
let hist = "HIST"
let ceres = "CERES"
let ecla = "ECLA"
let vetu = "VETU"
let dg = "DG"
let ens = "DENS"
let dri = "DRI"
let xt = "XT"

let string_of_key k =
    match String.lowercase_ascii k with
      | "info" -> "Informatique"
      | "dma" -> "Mathématiques"
      | "bio" | "ibens" -> "Biologie"
      | "phys" -> "Physique"
      | "chim" -> "Chimie"
      | "gsc" -> "Géosciences"
      | "phil" -> "Philosophie"
      | "dec" -> "Études cognitives"
      | "arts" -> "Arts"
      | "dsa" -> "Sciences de l'Antiquité"
      | "dss" -> "Sciences sociales"
      | "eco" -> "Économie"
      | "lila" -> "Langues anciennes"
      | "geog" -> "Géographie"
      | "hist" -> "Histoire"
      | "ceres" -> "Environnement et société"
      | "ecla" -> "Langues"
      | "vetu" -> "VETU"
      | "dg" -> "Délégation générale"
      | "dens" -> "Diplôme de l'ENS"
      | "dri" -> "Relations internationales"
      | _ -> "Autre"

let sciences = [info;dma;bio;phys;dec;gsc;chimie]
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
    activite,Activite;
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
  | Public_data.GEOSCIENCES -> gsc
  | Public_data.IBENS -> bio
  | Public_data.ECO -> eco
  | Public_data.DRI -> dri
  | Public_data.ARTS -> arts
  | Public_data.LILA -> lila
  | Public_data.DMA -> dma
  | Public_data.DEC -> dec

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
                            (Format.sprintf "Undefined GPS key: (%s) (%s)" code t)
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

let fold_repartition_diplome ~main_dpt ~firstname ~lastname ~f_nat ~f_dens state repartition dens =
  let state, dens =
      List.fold_left
          (fun (state, dens) course -> f_nat ~main_dpt ~firstname ~lastname (state, dens) course)
          (state,dens)
          repartition.Public_data.diplomes_nationaux
  in
  let state, dens =
      List.fold_left
          (fun (state, dens) course -> f_dens ~main_dpt ~firstname ~lastname (state, dens) course)
          (state,dens)
          repartition.Public_data.dens
  in
  state, dens

let f_gen get store ~main_dpt ~firstname ~lastname (state,dens) course =
    let year = course.Public_data.supplement_validation_year in
    let libelle = course.Public_data.supplement_intitule in
    let codegps = course.Public_data.supplement_code in
    let state, courselist =
        Remanent_state.get_sorted_courses ~firstname ~lastname ~year ~libelle ~codegps state
    in
    let courselist = List.filter (fun x -> not (x.Public_data.coursat_dpt = None)) courselist in
    let state, code =
    match courselist with
      | cours::_  ->
          begin
            match cours.Public_data.coursat_dpt with
            | None -> state, "xt"
            | Some a ->
                state, translate_main_dpt a
          end
      | [] ->
        begin
          let code = String.split_on_char '-' course.Public_data.supplement_code in
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
      end
    in
    if code = main_dpt then
      let state, (key,_kind) =
          kind_of_course state code course.Public_data.supplement_extra
      in
      let dens_cours_discipline_principale =    dens.Public_data.dens_cours_discipline_principale in
      let list = get dens_cours_discipline_principale in
      let course =
            {course with Public_data.supplement_discipline = string_of_key key}
      in
      let dens_cours_discipline_principale = store (course::list) dens_cours_discipline_principale in
      let dens = {dens with Public_data.dens_cours_discipline_principale} in
        state, dens
    else if code = xt then
        let state = Remanent_state.Course_to_be_sorted.add
                          state
                          {Public_data.coursat_nom = lastname;
                           Public_data.coursat_prenom = firstname;
                           Public_data.coursat_annee = year ;
                           Public_data.coursat_dpt = None ;
                           Public_data.coursat_libelle = libelle;
                           Public_data.coursat_codegps = codegps
                            }
                     in
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
      let dens_cours_langue = {course with Public_data.supplement_discipline="Langues"}::list in
      let dens = {dens with Public_data.dens_cours_langue} in
      state, dens
      | Activite ->
      let dens_cours_activite = dens.Public_data.dens_cours_activite in
      let course = {course with Public_data.supplement_discipline="Activité"} in
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
        let course =
            {course with Public_data.supplement_discipline = string_of_key key}
        in
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

let store_activite get set stage dens =
    let l = get dens in
    set (stage::l) dens

let store_activite_recherche stage dens =
    store_activite
      (fun dens -> dens.Public_data.dens_activite_recherche)
      (fun dens_activite_recherche dens -> {dens with Public_data.dens_activite_recherche})
      stage dens

let store_activite_internationale stage dens =
          store_activite
            (fun dens -> dens.Public_data.dens_activite_internationale)
            (fun dens_activite_internationale dens -> {dens with Public_data.dens_activite_internationale})
            stage dens

let store_activite_ouverture stage dens =
    store_activite
      (fun dens -> dens.Public_data.dens_activite_ouverture)
      (fun dens_activite_ouverture dens -> {dens with Public_data.dens_activite_ouverture})
      stage dens

let store_activite_autre stage dens =
    store_activite
    (fun dens -> dens.Public_data.dens_activite_autre)
    (fun dens_activite_autre dens -> {dens with Public_data.dens_activite_autre})
    stage dens

let fold_repartition_stages ~firstname ~lastname state stages dens =
    List.fold_left
      (fun (state,dens) stage ->
          match
            Remanent_state.get_sorted_internships ~firstname ~lastname  ~libelle:stage.Public_data.activite_intitule  state
          with
            | state, [] ->
              begin
                let dens_activite_a_trier = stage::dens.Public_data.dens_activite_a_trier in
                let dens = {dens with Public_data.dens_activite_a_trier} in
                let stage_entry =
                  {Public_data.stageat_nom=lastname;
                   Public_data.stageat_prenom=firstname;
                   Public_data.stageat_annee=stage.Public_data.activite_annee;
                   Public_data.stageat_libelle=
                      Tools.unsome_string stage.Public_data.activite_intitule_fr ;
                   Public_data.stageat_libelle_fr=
Tools.unsome_string stage.Public_data.activite_intitule_fr;
                   Public_data.stageat_libelle_en=stage.Public_data.activite_intitule_en;
                   Public_data.stageat_activite_fr=stage.Public_data.activite_activite_fr;
                   Public_data.stageat_activite_en=stage.Public_data.activite_activite_en;
                   Public_data.stageat_type=None}
                in
                let state =
                  Remanent_state.Internships_to_be_sorted.add state stage_entry
                in
                state, dens
              end
            | state, s::q ->
              begin
                let state =
                  match q with
                    | [] -> state
                    | _::_ ->
                        Remanent_state.warn
                          __POS__ "several internships match" Exit state
                in
                let activite_intitule_en = s.Public_data.stageat_libelle_en in
                let activite_intitule_fr = Some s.Public_data.stageat_libelle_fr in
                let activite_activite_en = s.Public_data.stageat_activite_en in
                let activite_activite_fr = s.Public_data.stageat_activite_fr in
                let stage =
                    {stage with
                        Public_data.activite_intitule_fr;
                        Public_data.activite_intitule_en;
                        Public_data.activite_activite_en;
                        Public_data.activite_activite_fr}
                in
                let dens =
                  match s.Public_data.stageat_type with
                    | Some Public_data.Recherche -> store_activite_recherche stage dens
                    | Some Public_data.Internationale -> store_activite_internationale stage dens
                    | Some Public_data.Ouverture ->
                    store_activite_ouverture stage dens
                    | Some Public_data.Hors_Dens -> dens
                    | None ->
                    store_activite_autre stage dens
                in
                state, dens
              end)
      (state,dens) stages

let split_courses ~firstname ~lastname dens state =
  let courses = dens.Public_data.dens_cours_a_trier in
  let dens = {dens with Public_data.dens_cours_a_trier = Public_data.empty_repartition_diplomes} in
  let state, main_dpt = Remanent_state.get_main_dpt state in
  let main_dpt = translate_main_dpt main_dpt in
  fold_repartition_diplome
      ~main_dpt ~firstname ~lastname
      ~f_nat ~f_dens
      state courses dens

let split_stages ~firstname ~lastname dens state =
    let stages = dens.Public_data.dens_activite_a_trier in
    let dens = {dens with Public_data.dens_activite_a_trier = []} in
    fold_repartition_stages
        ~firstname ~lastname
        state stages dens


let declare_as_minor dpt (state,dens) =
      let dpt  = translate_main_dpt dpt in
      match Public_data.StringMap.find_opt dpt  dens.Public_data.dens_cours_par_dpt
      with
      | None -> state, dens
      | Some a ->
          let dens_cours_par_dpt =
              Public_data.StringMap.remove dpt
                dens.Public_data.dens_cours_par_dpt
          in
          let dens_cours_mineure =
              Public_data.StringMap.add dpt
               a dens.Public_data.dens_cours_mineure
          in
          state, {dens with
                    Public_data.dens_cours_par_dpt;
                    Public_data.dens_cours_mineure}

let collect_mineure dens state =
    let firstname = dens.Public_data.dens_firstname in
    let lastname = dens.Public_data.dens_lastname in
    let year = dens.Public_data.dens_diplomation_year in
    let state, minors_list =
      Remanent_state.Collector_minor_candidate.find_list
        ~firstname ~lastname ~year
        state
    in
    List.fold_left
      (fun (state,dens) elt  ->
         match elt.Public_data.secondary_accepted with
           | None | Some false -> state, dens
           | Some true ->
                declare_as_minor elt.Public_data.secondary_dpt (state,dens)
      ) (state,dens) minors_list

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

  let dump_list_gen ?key get list (state,total) =
    let i,ects =
      List.fold_left
        (fun (i,ects) course ->
            (i+1,ects+.(get course)))
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

      let dump_list ?key = dump_list_gen ?key (fun course -> course.Public_data.supplement_ects)

      let dump_list_exp ?key = dump_list_gen ?key (fun course -> course.Public_data.activite_ects)


let add_total l =
    List.fold_left
        (fun (i,i',ects,ects') (j,j',fcts,fcts') ->
                (i+j,i'+j',ects+.fcts,ects'+.fcts'))
        (0,0,0.,0.) l

let display_exp state label l =
    let n = List.length l in
    if n=0 then ()
    else Remanent_state.fprintf state "%s (%i)," label n

let count_exp dens =
    List.fold_left
      (fun n l  -> n+List.length l)
      0
      [
        dens.Public_data.dens_activite_ouverture;
        dens.Public_data.dens_activite_recherche;
        dens.Public_data.dens_activite_internationale;
        dens.Public_data.dens_activite_autre
      ]

let print_check state =
    let () = Remanent_state.fprintf state "\\textcolor{darkgreen}{\\CheckmarkBold}" in
    state
let print_in_progress state =
    let () = Remanent_state.fprintf state "\\textcolor{orange}{\\ldots}" in
    state
let print_status bool state =
    if bool
    then print_check state
    else print_in_progress state

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
    let liste = dens.Public_data.dens_cours_mineure in
    let state,total_minor  =
        Public_data.StringMap.fold
          (fun key  -> dump_repartition ~key:(String.uppercase_ascii key))
          liste (state,total_init)
    in
    let liste = dens.Public_data.dens_cours_majeure in
    let state,total_major  =
        Public_data.StringMap.fold
          (fun key  -> dump_repartition ~key:(String.uppercase_ascii key))
          liste (state,total_init)
    in
    let total_other = add_total [total_other;total_minor;total_major] in
    let l1, l2, l3, l4, l5 = dens.Public_data.dens_activite_internationale,
                         dens.Public_data.dens_activite_recherche,
                         dens.Public_data.dens_activite_ouverture,
                         dens.Public_data.dens_activite_autre,
                         dens.Public_data.dens_activite_a_trier
    in
    let liste = List.flatten [l1;l2;l3;l4;l5] in
    let state, total_exp =
        dump_list_exp ~key:"Expériences"
              liste (state, total_init)
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
     let total = add_total [total_to_sort; total_resp; total_ecla; total_other;  total_principale; total_exp] in
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
      let state = print_status (dens.Public_data.dens_nb_inscriptions > 2) state in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "Sortant : %s (doit être sortant)" (match dens.Public_data.dens_sortant with
          | None -> "non précisé"
          | Some true -> "Oui"
          | Some false -> "Non") in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "ECTS DENS : %s (72 sont nécessaires)" (string_of_float ects') in
      let state = print_status (ects'>=72.) state in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state
                  "ECTS discipline principale : %s (24 sont nécessaires)"
                  (let (_,_,_,ects')=total_principale in (string_of_float ects'))
      in
      let state = print_status (let (_,_,_,ects')=total_principale in ects'>=24.) state in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state
                  "ECTS autres disciplines : %s (24 sont nécessaires)"
                  (let (_,_,_,ects')=total_other in (string_of_float ects'))
      in
      let state = print_status (let (_,_,_,ects')=total_other in ects'>=24.) state in
      let () = Remanent_state.print_newline state in
    (*  let () = Remanent_state.fprintf state
                  "ECTS langues : %s (24 sont nécessaires%s)"
                  (let (_,_,_,ects')=total_ecla in (string_of_float ects'))
                  (match main_dpt with | Public_data.DI -> " ou un stage à l'étranger" | Public_data.DMA | Public_data.ENS|Public_data.PHYS|Public_data.GEOSCIENCES | Public_data.DEC |  Public_data.CHIMIE|Public_data.IBENS|Public_data.ECO|Public_data.DRI|Public_data.ARTS|Public_data.LILA -> "")
      in
      let () = Remanent_state.print_newline state in *)
      let () = Remanent_state.fprintf state "Expériences : " in
      let () = display_exp state "Ouverture" dens.Public_data.dens_activite_ouverture in
      let () = display_exp state "Recherche" dens.Public_data.dens_activite_recherche in
      let () = display_exp state "Internationale" dens.Public_data.dens_activite_internationale in
      let () = display_exp state "Autre"
      dens.Public_data.dens_activite_autre in
      let state = print_status (count_exp dens > 1)  state in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "M2 recherche : " in
      let () = (match dens.Public_data.dens_master with
               | [] -> let () = Remanent_state.fprintf state "aucun" in
                       ()
               | l ->
                 let () = List.iter
                   (fun dpl ->
                       Remanent_state.fprintf state "%s ; "
                         (Public_data.label_of_diplome dpl)) (List.rev l)
                in ())
      in
      let () = Remanent_state.fprintf state " (M2 recherche en %s obligatoire)"
          (match main_dpt with
            Public_data.DMA -> "mathématiques"
          | Public_data.DI -> "informatique"
          | Public_data.CHIMIE -> "chimie"
          | Public_data.DEC -> "sciences cognitives"
          | Public_data.GEOSCIENCES -> "géosciences"
          | Public_data.IBENS -> "biologie"
          | Public_data.PHYS -> "physique"
          | Public_data.ECO -> "économie"
          | Public_data.LILA -> "langues anciennes"
          | Public_data.ARTS -> "arts"
          | (Public_data.ENS|Public_data.DRI)
  -> "informatique") in
      let state =
          print_status
            (List.exists
                (fun a -> a.Public_data.diplome_dpt = main_dpt)
              dens.Public_data.dens_master) state
      in
      let () = Remanent_state.print_newline state in
      let () = Remanent_state.fprintf state "Diplômes autre : " in
      let () = (match dens.Public_data.dens_parcours with
               | [] -> Remanent_state.fprintf state "aucun"
               | l ->
                 List.iter
                   (fun dpl -> Remanent_state.fprintf state "%s ; "
                         (Public_data.label_of_diplome dpl)) (List.rev l))
      in
      let () = Remanent_state.print_newline state in
      let state =
        match main_dpt with
        | Public_data.DI ->
           begin
               let () = Remanent_state.fprintf state "Cours obligatoires : %i (5 sont nécessaires)" dens.Public_data.dens_nb_mandatory_course in
               let state = print_status (dens.Public_data.dens_nb_mandatory_course > 4) state in
              let () = Remanent_state.print_newline state in
           let () = Remanent_state.fprintf
                      state
                      "Cours de maths: %i "
                      dens.Public_data.dens_nb_math_course
           in
           let () = Remanent_state.print_newline state in
           let () = Remanent_state.fprintf
                    state
                    "Cours de maths-info : %i (2 cours de maths ou maths-info  nécessaires dont au moins un de maths)"
                    (dens.Public_data.dens_nb_math_and_math_info_course - dens.Public_data.dens_nb_math_course)
           in
           let state = print_status (dens.Public_data.dens_nb_math_course > 0 &&
dens.Public_data.dens_nb_math_and_math_info_course > 1) state in
           let () = Remanent_state.print_newline state in
           state
           end
           | Public_data.DMA
           | Public_data.ENS | Public_data.GEOSCIENCES
           | Public_data.PHYS | Public_data.CHIMIE | Public_data.DEC |  Public_data.IBENS|Public_data.ECO|Public_data.DRI|Public_data.ARTS|Public_data.LILA -> state
      in
      let () = Remanent_state.fprintf state "\\end{minipage}" in
      let () = Remanent_state.fprintf state "\\end{center}" in
      let () = Remanent_state.fprintf state "\\vfill\\mbox{}" in
  state

let suggest_mineure dens state =
  let liste = dens.Public_data.dens_cours_par_dpt in
  let firstname = dens.Public_data.dens_firstname in
  let lastname = dens.Public_data.dens_lastname in
  let year = dens.Public_data.dens_diplomation_year in
  let state, minors_list =
      Remanent_state.Collector_minor_candidate.find_list
        ~firstname ~lastname ~year
        state
  in
  let state, map' =
      List.fold_left
          (fun (state,map') a ->
              state, Public_data.StringMap.add
                (translate_main_dpt a.Public_data.secondary_dpt)
                a map'
            )
          (state,Public_data.StringMap.empty) minors_list
  in
  let map', state =
    Public_data.StringMap.fold
      (fun key elt (map',state) ->
          let map', accepted =
              match Public_data.StringMap.find_opt key map' with
                | None -> map', None
                | Some a -> Public_data.StringMap.remove key map', a.Public_data.secondary_accepted
          in
          let key = String.uppercase_ascii key in
          let ects =
              List.fold_left
                (fun ects course ->
                    ects+.course.Public_data.supplement_ects)
                0. elt.Public_data.diplomes_nationaux
          in
          let year_int = try int_of_string year with _ -> 0 in
          let ects =
              List.fold_left
                (fun ects course ->
                    ects+.course.Public_data.supplement_ects)
              ects elt.Public_data.dens
          in
          if
          ((ects >= 30. && year_int >= 2024) ||
          (List.mem key humanities && ects >= 48.) ||
          (List.mem key sciences && ects >= 24.) ||
          (accepted = Some true))
            && (not (accepted = Some false))
          then
            let m =
            {
             Public_data.secondary_student_lastname=lastname;
             Public_data.secondary_student_firstname=firstname;
             Public_data.secondary_student_promo=dens.Public_data.dens_promotion;
             Public_data.secondary_dpt = Public_data.dpt_of_string key ;
             Public_data.secondary_diplomation_year = year;
             Public_data.secondary_accepted = accepted}
            in
            map', Remanent_state.Dens_candidate_missing_minors.add state m
        else map', state)
      liste  (map',state)
in
let state =
Public_data.StringMap.fold
  (fun key elt state ->
      let m =
        {
         Public_data.secondary_student_lastname=lastname;
         Public_data.secondary_student_firstname=firstname;
         Public_data.secondary_student_promo=dens.Public_data.dens_promotion;
         Public_data.secondary_dpt = Public_data.dpt_of_string key ;
         Public_data.secondary_diplomation_year = year ;
         Public_data.secondary_accepted = elt.Public_data.secondary_accepted}
        in
        Remanent_state.Dens_candidate_missing_minors.add state m)
  map' state
in
state


let suggest_majeure dens state =
  let l1 = dens.Public_data.dens_master in
  let l2 = dens.Public_data.dens_parcours in
  let add m a =
      let dpt = a.Public_data.diplome_dpt in
      Public_data.DptMap.add
          dpt (match Public_data.DptMap.find_opt dpt m
             with
              | None -> 1
              | Some n -> n+1)
          m
  in
  let map =
      List.fold_left add (List.fold_left add Public_data.DptMap.empty l2) l1
  in
  let firstname = dens.Public_data.dens_firstname in
  let lastname = dens.Public_data.dens_lastname in
  let year = dens.Public_data.dens_diplomation_year in
  let state, majors_list =
      Remanent_state.Collector_major_candidate.find_list
        ~firstname ~lastname ~year
        state
  in
  let state, map' =
      List.fold_left
          (fun (state,map') a ->
              state, Public_data.DptMap.add
                a.Public_data.secondary_dpt
                a map'
            )
          (state,Public_data.DptMap.empty) majors_list
  in
  let map', state =
    Public_data.DptMap.fold
      (fun key elt (map',state) ->
          let map', accepted =
              match Public_data.DptMap.find_opt key map' with
                | None -> map', None
                | Some a -> Public_data.DptMap.remove key map', a.Public_data.secondary_accepted
          in
          if
            ((elt > 1 && key <> dens.Public_data.dens_main_dpt)  || (accepted = Some true))
            && (not (accepted = Some false))
          then
            let m =
            {
             Public_data.secondary_student_lastname=lastname;
             Public_data.secondary_student_firstname=firstname;
             Public_data.secondary_student_promo=dens.Public_data.dens_promotion;
             Public_data.secondary_dpt = key ;
             Public_data.secondary_diplomation_year = year;
             Public_data.secondary_accepted = accepted}
            in
            map', Remanent_state.Dens_candidate_missing_majors.add state m
        else map', state)
      map  (map',state)
in
let state =
Public_data.DptMap.fold
  (fun key elt state ->
      let m =
        {
         Public_data.secondary_student_lastname=lastname;
         Public_data.secondary_student_firstname=firstname;
         Public_data.secondary_student_promo=dens.Public_data.dens_promotion;
         Public_data.secondary_dpt = key ;
         Public_data.secondary_diplomation_year = year ;
         Public_data.secondary_accepted = elt.Public_data.secondary_accepted}
        in
        Remanent_state.Dens_candidate_missing_majors.add state m)
  map' state
in
state

let suggest_candidate dens state =
    if
      (dens.Public_data.dens_nb_inscriptions>=3 )
      (*&& (match dens.Public_data.dens_sortant with
            | Some false -> false | _ -> true)
      && (dens.Public_data.dens_total_ects>=72.)
      && (match dens.Public_data.dens_master with [] -> false | _ -> true)*)
    then
      let s =
           {
      Public_data.dens_candidate_main_dpt = dens.Public_data.dens_main_dpt ;
      Public_data.dens_candidate_firstname = dens.Public_data.dens_firstname ;
      Public_data.dens_candidate_lastname = dens.Public_data.dens_lastname ;
      Public_data.dens_candidate_promotion = dens.Public_data.dens_promotion ;
      Public_data.dens_candidate_diplomation_year = dens.Public_data.dens_diplomation_year ;
      Public_data.dens_candidate_ok = dens.Public_data.dens_ok;
      Public_data.dens_candidate_ine = dens.Public_data.dens_ine;
      Public_data.dens_candidate_sad = dens.Public_data.dens_sad
      }
      in
      Remanent_state.Dens_candidate_suggestion.add  state s
    else state

let repeatable state cours extra =
  match kind_of_course state cours extra with
      | state , (_,(Activite | Dummy | Missing  )) -> state, true
      | state, (_,( Humanities | Ecla
        | Sciences
        | Sans_mineure)) -> state, false


(* Collect dens candidates from the data-bases *)
type dens_candidate_id =
  {
    candidate_main_dpt: Public_data.main_dpt option ;
    candidate_firstname : string option ;
    candidate_lastname : string option ;
    candidate_promotion : string option ;
    candidate_diplomation_year : string option ;
    candidate_ok : bool option ;
    candidate_ine : string option ;
    candidate_sad : int option ;
}

let empty_candidate_id =
{
  candidate_main_dpt =  None ;
  candidate_firstname = None ;
  candidate_lastname = None ;
  candidate_promotion = None ;
  candidate_diplomation_year = None ;
  candidate_ok = None ;
  candidate_ine = None ;
  candidate_sad = None ;
}

let collect_bool suffix pos state =
  Tools.collect_bool
    (fun msg state ->
       let msg = msg^suffix in
       Remanent_state.warn
         pos
         msg
         Exit
         state
    )
    state

    let collect_int suffix pos state =
      Tools.collect_int
        (fun msg state ->
           let msg = msg^suffix in
           Remanent_state.warn
             pos
             msg
             Exit
             state
        )
        state

let event_opt = Some (Profiling.Collect_dens_candidates)
let compute_repository =
Remanent_state.Collector_dens_candidate.get_repository
(* Remanent_state.get_dens_candidates_list_repository*)

let lift_pred = Lift.pred_safe
let lift_string =
  (Lift.string empty_candidate_id Public_data.empty_dens_candidate).Lift.safe
let lift_string_opt =
  (Lift.string empty_candidate_id Public_data.empty_dens_candidate).Lift.opt_safe
let lift_bool_opt =
  (Lift.bool empty_candidate_id Public_data.empty_dens_candidate).Lift.opt_safe
let lift_dpt =
  (Lift.main_dpt empty_candidate_id Public_data.empty_dens_candidate).Lift.safe
  let lift_int_opt =
    (Lift.int empty_candidate_id Public_data.empty_dens_candidate).Lift.opt_safe

let keywords_list =
  [
    Public_data.Ignore ;
    Public_data.FirstName;
    Public_data.LastName;
    Public_data.Annee_Academique;
    Public_data.Promo;
    Public_data.Departement;
    Public_data.Accepte;
    Public_data.Numero_ine;
    Public_data.Numero_sad;
  ]

  let keywords_of_interest =
    [
      Public_data.FirstName;
      Public_data.LastName;
      Public_data.Annee_Academique;
      Public_data.Promo;
      Public_data.Departement;
      Public_data.Numero_ine;
      Public_data.Numero_sad;
    ]

let mandatory_fields =
      [
        lift_pred (fun a -> a.candidate_diplomation_year) "diplomation year";
        lift_pred (fun a -> a.candidate_firstname) "the first name of the student";
        lift_pred (fun a -> a.candidate_lastname) "the last name of the student";
        lift_pred (fun a -> a.candidate_promotion) "promotion";

        lift_pred (fun a -> a.candidate_main_dpt) "the department";
      ]

let all_fields =
    let record_name = "dens candidate" in
        [
          lift_string
            ~keyword:Public_data.FirstName
            ~set_tmp:(Tools.collect_string
                        (fun candidate_firstname x ->
                              {x with candidate_firstname}))
            ~get_tmp:(fun a -> a.candidate_firstname)
            ~get:(fun a -> a.Public_data.dens_candidate_firstname)
            ~set:(fun dens_candidate_firstname a ->
               {a with Public_data.dens_candidate_firstname})
            ~record_name
            ~field_name:"first name of the student"
            ~pos:__POS__ ;
          lift_string
            ~keyword:Public_data.LastName
            ~set_tmp:(Tools.collect_string
                          (fun candidate_lastname x ->
                                {x with candidate_lastname}))
            ~get_tmp:(fun a -> a.candidate_lastname)
            ~get:(fun a -> a.Public_data.dens_candidate_lastname)
            ~set:(fun dens_candidate_lastname a ->
                {a with Public_data.dens_candidate_lastname})
            ~record_name
            ~field_name:"last name of the student"
            ~pos:__POS__ ;


          lift_string
            ~keyword:Public_data.Annee_Academique
            ~set_tmp:(Tools.collect_string
                  (fun candidate_diplomation_year x ->
                      {x with candidate_diplomation_year}))
            ~get_tmp:(fun a -> a.candidate_diplomation_year)
            ~get:(fun a -> a.Public_data.dens_candidate_diplomation_year)
            ~set:(fun dens_candidate_diplomation_year a ->
                {a with Public_data.dens_candidate_diplomation_year})
            ~record_name
            ~field_name:"diplomation year of the student"
            ~pos:__POS__;
          lift_dpt
            ~keyword:Public_data.Departement
            ~set_tmp:(Tools.collect_string (fun dpt x ->
                let candidate_main_dpt =
                  Tools.map_opt
                    Public_data.dpt_of_string dpt
                in {x with candidate_main_dpt}))
            ~get_tmp:(fun a -> a.candidate_main_dpt)
            ~get:(fun a -> a.Public_data.dens_candidate_main_dpt)
            ~set:(fun dens_candidate_main_dpt a ->
                {a with Public_data.dens_candidate_main_dpt})
            ~record_name
            ~field_name:"department of the student"
            ~pos:__POS__ ;

            lift_string
              ~keyword:Public_data.Promo
              ~set_tmp:(Tools.collect_string (fun candidate_promotion x -> {x with candidate_promotion}))
              ~get_tmp:(fun a -> a.candidate_promotion)
              ~get:(fun a -> a.Public_data.dens_candidate_promotion)
              ~set:(fun dens_candidate_promotion a ->
                  {a with Public_data.dens_candidate_promotion})
              ~record_name
              ~field_name:"Promo of the student"
              ~pos:__POS__;
          lift_bool_opt
            ~keyword:Public_data.Accepte
            ~set_tmp:(collect_bool "in validation" __POS__
                        (fun candidate_ok x -> {x with candidate_ok}))
            ~get_tmp:(fun a -> a.candidate_ok)
            ~get:(fun a -> a.Public_data.dens_candidate_ok)
            ~set:(fun dens_candidate_ok a ->
                {a with Public_data.dens_candidate_ok})
            ~field_name:"validation"
            ~record_name
            ~pos:__POS__;
            lift_string_opt
              ~keyword:Public_data.Numero_ine
              ~set_tmp:(Tools.collect_string
                          (fun candidate_ine x -> {x with candidate_ine}))
              ~get_tmp:(fun a -> a.candidate_ine)
              ~get:(fun a -> a.Public_data.dens_candidate_ine)
              ~set:(fun dens_candidate_ine a ->
                  {a with Public_data.dens_candidate_ine})
              ~field_name:"INE"
              ~record_name
              ~pos:__POS__;
              lift_int_opt
                ~keyword:Public_data.Numero_sad
                ~set_tmp:(collect_int"sad" __POS__
                            (fun candidate_sad x -> {x with candidate_sad}))
                ~get_tmp:(fun a -> a.candidate_sad)
                ~get:(fun a -> a.Public_data.dens_candidate_sad)
                ~set:(fun dens_candidate_sad a ->
                    {a with Public_data.dens_candidate_sad})
                ~field_name:"Numeror SAD"
                ~record_name
                ~pos:__POS__;
            ]

let get_dens_candidates
    ?repository
    ?prefix
    ?file_name
    state
    =
    let state, str = compute_repository state in
    let event = Some (Profiling.Scan_csv_files (str,"")) in
    let state = Remanent_state.open_event_opt event state in
    let state = Scan_csv_files.collect_gen
      ~strict:true
      ?repository
      ?prefix
      ?file_name
      ~compute_repository
      ~fun_default:Tools.fun_ignore
      ~keywords_of_interest
      ~keywords_list
      ~init_state:empty_candidate_id
      ~empty_elt:Public_data.empty_dens_candidate            ~add_elt:Remanent_state.Collector_dens_candidate.add
      ~mandatory_fields
      ~all_fields
      ?event_opt
      state
    in
    let state = Remanent_state.close_event_opt event state in
    state

    (* Collect minors candidates from the data-bases *)
    type secondary_id =
      {
        secondary_dpt: Public_data.main_dpt option ;
        secondary_firstname : string option ;
        secondary_lastname : string option ;
        secondary_promotion : string option ;
        secondary_diplomation_year : string option ;
        secondary_ok : bool option ;
    }

    let empty_secondary_id =
    {
      secondary_dpt =  None ;
      secondary_firstname = None ;
      secondary_lastname = None ;
      secondary_promotion = None ;
      secondary_diplomation_year = None ;
      secondary_ok = None ;
    }

    let event_opt = Some (Profiling.Collect_minors)
    let compute_repository =   Remanent_state.Collector_minor_candidate.get_repository


    let keywords_list =
      [
        Public_data.Ignore ;
        Public_data.FirstName;
        Public_data.LastName;
        Public_data.Annee_Academique;
        Public_data.Promo;
        Public_data.Departement;
        Public_data.Accepte;
      ]

      let keywords_of_interest =
        [
          Public_data.FirstName;
          Public_data.LastName;
          Public_data.Annee_Academique;
          Public_data.Promo;
          Public_data.Departement;
        ]

        let lift_string =
          (Lift.string empty_secondary_id Public_data.empty_mineure_majeure).Lift.safe
        (*let lift_string_opt =
          (Lift.string empty_candidate_id Public_data.empty_dens_candidate).Lift.opt_safe*)
        let lift_bool_opt =
          (Lift.bool empty_secondary_id Public_data.empty_mineure_majeure).Lift.opt_safe
        let lift_dpt =
          (Lift.main_dpt empty_secondary_id Public_data.empty_mineure_majeure).Lift.safe


    let mandatory_fields =
          [
            lift_pred (fun a -> a.secondary_diplomation_year) "diplomation year";
            lift_pred (fun a -> a.secondary_firstname) "the first name of the student";
            lift_pred (fun a -> a.secondary_lastname) "the last name of the student";
            lift_pred (fun a -> a.secondary_promotion) "promotion";
            lift_pred (fun a -> a.secondary_dpt) "fields of the minor";
          ]

    let all_fields =
        let record_name = "dens minor candidate" in
            [
              lift_string
                ~keyword:Public_data.FirstName
                ~set_tmp:(Tools.collect_string
                            (fun secondary_firstname x ->
                                  {x with secondary_firstname}))
                ~get_tmp:(fun a -> a.secondary_firstname)
                ~get:(fun a -> a.Public_data.secondary_student_firstname)
                ~set:(fun secondary_student_firstname a ->
                   {a with Public_data.secondary_student_firstname})
                ~record_name
                ~field_name:"first name of the student"
                ~pos:__POS__ ;
              lift_string
                ~keyword:Public_data.LastName
                ~set_tmp:(Tools.collect_string
                              (fun secondary_lastname x ->
                                    {x with secondary_lastname}))
                ~get_tmp:(fun a -> a.secondary_lastname)
                ~get:(fun a -> a.Public_data.secondary_student_lastname)
                ~set:(fun secondary_student_lastname a ->
                    {a with Public_data.secondary_student_lastname})
                ~record_name
                ~field_name:"last name of the student"
                ~pos:__POS__ ;


              lift_string
                ~keyword:Public_data.Annee_Academique
                ~set_tmp:(Tools.collect_string
                      (fun secondary_diplomation_year x ->
                          {x with secondary_diplomation_year}))
                ~get_tmp:(fun a -> a.secondary_diplomation_year)
                ~get:(fun a -> a.Public_data.secondary_diplomation_year)
                ~set:(fun secondary_diplomation_year a ->
                    {a with Public_data.secondary_diplomation_year})
                ~record_name
                ~field_name:"diplomation year of the student"
                ~pos:__POS__;
              lift_dpt
                ~keyword:Public_data.Departement
                ~set_tmp:(Tools.collect_string (fun dpt x ->
                    let secondary_dpt =
                      Tools.map_opt
                        Public_data.dpt_of_string dpt
                    in {x with secondary_dpt}))
                ~get_tmp:(fun a -> a.secondary_dpt)
                ~get:(fun a -> a.Public_data.secondary_dpt)
                ~set:(fun secondary_dpt a ->
                    {a with Public_data.secondary_dpt})
                ~record_name
                ~field_name:"department of the minor/major"
                ~pos:__POS__ ;

                lift_string
                  ~keyword:Public_data.Promo
                  ~set_tmp:(Tools.collect_string (fun secondary_promotion x -> {x with secondary_promotion}))
                  ~get_tmp:(fun a -> a.secondary_promotion)
                  ~get:(fun a -> a.Public_data.secondary_student_promo)
                  ~set:(fun secondary_student_promo a ->
                      {a with Public_data.secondary_student_promo})
                  ~record_name
                  ~field_name:"Promo of the student"
                  ~pos:__POS__;
              lift_bool_opt
                ~keyword:Public_data.Accepte
                ~set_tmp:(collect_bool "in validation" __POS__
                            (fun secondary_ok x -> {x with secondary_ok}))
                ~get_tmp:(fun a -> a.secondary_ok)
                ~get:(fun a -> a.Public_data.secondary_accepted)
                ~set:(fun secondary_accepted a ->
                    {a with Public_data.secondary_accepted})
                ~field_name:"validation"
                ~record_name
                ~pos:__POS__;
                ]

    let get_mineures_candidates
        ?repository
        ?prefix
        ?file_name
        state
        =
        let state, str = compute_repository state in
        let event = Some (Profiling.Scan_csv_files (str,"")) in
        let state = Remanent_state.open_event_opt event state in
        let state = Scan_csv_files.collect_gen
          ~strict:true
          ?repository
          ?prefix
          ?file_name
          ~compute_repository
          ~fun_default:Tools.fun_ignore
          ~keywords_of_interest
          ~keywords_list
          ~init_state:empty_secondary_id
          ~empty_elt:Public_data.empty_mineure_majeure   ~add_elt:Remanent_state.Collector_minor_candidate.add
          ~mandatory_fields
          ~all_fields
          ?event_opt
          state
        in
        let state = Remanent_state.close_event_opt event state in
        state

        let event_opt = Some (Profiling.Collect_majors)
        let compute_repository =
          Remanent_state.Collector_major_candidate.get_repository

        let all_fields =
            let record_name = "dens major candidate" in
                [
                  lift_string
                    ~keyword:Public_data.FirstName
                    ~set_tmp:(Tools.collect_string
                                (fun secondary_firstname x ->
                                      {x with secondary_firstname}))
                    ~get_tmp:(fun a -> a.secondary_firstname)
                    ~get:(fun a -> a.Public_data.secondary_student_firstname)
                    ~set:(fun secondary_student_firstname a ->
                       {a with Public_data.secondary_student_firstname})
                    ~record_name
                    ~field_name:"first name of the student"
                    ~pos:__POS__ ;
                  lift_string
                    ~keyword:Public_data.LastName
                    ~set_tmp:(Tools.collect_string
                                  (fun secondary_lastname x ->
                                        {x with secondary_lastname}))
                    ~get_tmp:(fun a -> a.secondary_lastname)
                    ~get:(fun a -> a.Public_data.secondary_student_lastname)
                    ~set:(fun secondary_student_lastname a ->
                        {a with Public_data.secondary_student_lastname})
                    ~record_name
                    ~field_name:"last name of the student"
                    ~pos:__POS__ ;


                  lift_string
                    ~keyword:Public_data.Annee_Academique
                    ~set_tmp:(Tools.collect_string
                          (fun secondary_diplomation_year x ->
                              {x with secondary_diplomation_year}))
                    ~get_tmp:(fun a -> a.secondary_diplomation_year)
                    ~get:(fun a -> a.Public_data.secondary_diplomation_year)
                    ~set:(fun secondary_diplomation_year a ->
                        {a with Public_data.secondary_diplomation_year})
                    ~record_name
                    ~field_name:"diplomation year of the student"
                    ~pos:__POS__;
                  lift_dpt
                    ~keyword:Public_data.Departement
                    ~set_tmp:(Tools.collect_string (fun dpt x ->
                        let secondary_dpt =
                          Tools.map_opt
                            Public_data.dpt_of_string dpt
                        in {x with secondary_dpt}))
                    ~get_tmp:(fun a -> a.secondary_dpt)
                    ~get:(fun a -> a.Public_data.secondary_dpt)
                    ~set:(fun secondary_dpt a ->
                        {a with Public_data.secondary_dpt})
                    ~record_name
                    ~field_name:"department of the minor/major"
                    ~pos:__POS__ ;

                    lift_string
                      ~keyword:Public_data.Promo
                      ~set_tmp:(Tools.collect_string (fun secondary_promotion x -> {x with secondary_promotion}))
                      ~get_tmp:(fun a -> a.secondary_promotion)
                      ~get:(fun a -> a.Public_data.secondary_student_promo)
                      ~set:(fun secondary_student_promo a ->
                          {a with Public_data.secondary_student_promo})
                      ~record_name
                      ~field_name:"Promo of the student"
                      ~pos:__POS__;
                  lift_bool_opt
                    ~keyword:Public_data.Accepte
                    ~set_tmp:(collect_bool "in validation" __POS__
                                (fun secondary_ok x -> {x with secondary_ok}))
                    ~get_tmp:(fun a -> a.secondary_ok)
                    ~get:(fun a -> a.Public_data.secondary_accepted)
                    ~set:(fun secondary_accepted a ->
                        {a with Public_data.secondary_accepted})
                    ~field_name:"validation"
                    ~record_name
                    ~pos:__POS__;
                    ]

        let get_majeures_candidates
            ?repository
            ?prefix
            ?file_name
            state
            =
            let state, str = compute_repository state in
            let event = Some (Profiling.Scan_csv_files (str,"")) in
            let state = Remanent_state.open_event_opt event state in
            let state = Scan_csv_files.collect_gen
              ~strict:true
              ?repository
              ?prefix
              ?file_name
              ~compute_repository
              ~fun_default:Tools.fun_ignore
              ~keywords_of_interest
              ~keywords_list
              ~init_state:empty_secondary_id
              ~empty_elt:Public_data.empty_mineure_majeure   ~add_elt:Remanent_state.Collector_major_candidate.add
              ~mandatory_fields
              ~all_fields
              ?event_opt
              state
            in
            let state = Remanent_state.close_event_opt event state in
            state
