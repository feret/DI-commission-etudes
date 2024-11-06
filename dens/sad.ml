let times = ["double";"triple";"quadruple";"quintuple"]
let ordre = ["première";"seconde";"troisième";"quatrième";"cinquième"]
let order = ["first";"second";"third";"fourth";"fifth"]

let string_of_float x = if x=0. then "" else Notes.string_of_ects (Some x)

let title = "\\textbf{SUPPLÉMENT AU DIPLÔME DE L'ÉCOLE NORMALE SUPÉRIEURE}"
let birthdate dens =
    match dens.Public_data.dens_birthdate with
      | None -> ""
      | Some i -> i
let ine dens =
    match dens.Public_data.dens_ine with
      | None -> ""
      | Some i -> i

let sad dens =
match dens.Public_data.dens_sad with
  | None -> ""
  | Some i -> Format.sprintf "%i" i

let print_preamble state dens =
    let state, enspsl =
        Remanent_state.get_ENSPSL_logo_bis state
    in
    let enspsl = List.rev_map Special_char.trans_latex_address (List.rev enspsl) in
    let f x =
      Printf.sprintf
        "{\\includegraphics[width=5cm]{%s}}\\mbox{}"
        x
    in
    let state, s  =
      Tools.include_latex_list
        f
        state
        enspsl
    in
    let () =
        Remanent_state.fprintf_verbatim state "\\lhead{%s}" s
    in
    let () = Remanent_state.print_newline state in
    let () =
        Remanent_state.fprintf state "\\rhead{Supplément au diplôme de l'étudiant N$^o\\;$%s}" (sad dens)  in
    let () = Remanent_state.print_newline state in
    let () =
        Remanent_state.fprintf state "\\cfoot{}"
    in
    let () = Remanent_state.print_newline state in
    let () =
        Remanent_state.fprintf state "\\rfoot{\\textit{Supplément au diplôme de l'ENS - page \\thepage/\\pageref{LastPage}}}"
    in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\newcommand{\\percent}{$\\%s$}" "%" in
   let () = Remanent_state.fprintf state "\\title{%s}" title in
   let () = Remanent_state.print_newline state in
   let () = Remanent_state.fprintf state "\\date{}"  in
   let () = Remanent_state.print_newline state in
   let () = Remanent_state.fprintf state "\\maketitle" in
   let () = Remanent_state.print_newline state in
   let () = Remanent_state.fprintf state "\\thispagestyle{fancy}" in
   let () = Remanent_state.print_newline state in
   let () = Remanent_state.fprintf state
        "La présente annexe descriptive au diplôme (supplément au diplôme) suit le modèle élaboré par la Commission européenne, le Conseil de l'Europe et l'UNESCO/CEPES." in
   let () = Remanent_state.print_newline state in
   let () = Remanent_state.fprintf state "Elle vise à fournir des données indépendantes et suffisantes pour améliorer la transparence internationale et la reconnaissance académique et professionnelle équitable des qualifications (diplômes, acquis universitaires, certificats, etc.).%%\n" in
   let () = Remanent_state.fprintf state "Elle est destinée à décrire la nature, le niveau, le contexte, le contenu et le statut des études accomplies avec succès par la personne désignée par la qualification originale à laquelle ce présent supplément est annexé.%%\n" in
   let () = Remanent_state.fprintf state "Elle doit être dépourvue de tout jugement de valeur, déclaration d'équivalence ou suggestion de reconnaissance. Toutes les informations requises dans cette annexe doivent être fournies. Lorsqu'une information fait défaut, une explication doit être donnée.%%\n" in
   let () = Remanent_state.fprintf state "\\section*{Principes généraux du diplôme de l'ENS}" in
   let () = Remanent_state.print_newline state in
   let () = Remanent_state.fprintf state
      "La formation reçue à l'École normale supérieure s'inscrit dans le schéma général européen du \" processus de Bologne \", dit système 3-5-8 ou LMD. %%\n " in
   let () = Remanent_state.fprintf state "À ce titre, le pivot du diplôme de l'ENS est le diplôme national de Master. %%\n" in
  let () = Remanent_state.fprintf state "Le rôle dévolu au diplôme d'établissement est d'identifier et de valoriser la formation spécifique donnée par l'ENS grâce aux multiples itinéraires intellectuels qu'elle permet.%%\n" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state
      "La formation à l'ENS est avant tout une formation par la recherche. Elle accorde ainsi une place importante à l'initiative personnelle et à l'autonomie scientifique dont les diverses expressions (y compris les stages de recherche, sous certaines conditions) sont validées dans le cadre du diplôme. %%\n" in
    let () = Remanent_state.fprintf state "La recherche ayant une portée résolument internationale, les cursus proposés incluent ou permettent un séjour ou un stage à l'étranger de longue durée (6 mois à un an)." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state
      "Une richesse majeure de l'École normale supérieure, l'une des rares écoles en France où cohabitent des étudiant.e.s littéraires et scientifiques, réside de surcroît dans une véritable pluridisciplinarité, à laquelle le diplôme réserve une place importante." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state
      "Enfin, comme dans un véritable campus universitaire, étudiants et enseignants se côtoient régulièrement~; les normaliens et normaliennes sont suivi.e.s tout au long de leur scolarité par un tuteur ou une tutrice qui les conseille et les guide dans leurs choix, dans une relation de confiance et d'engagement mutuel." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\section*{Conditions d'obtention du diplôme}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "L'obtention du diplôme de l'ENS est subordonnée à la validation~:" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\begin{itemize}[leftmargin=0.3cm]" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\item[-]  d'une formation principale sanctionnée par l'obtention d'un master à orientation recherche. Un master professionnel comportant un mémoire de recherche peut éventuellement se substituer au master recherche. La discipline ou la mention et la spécialité de ce master définissent la \" spécialité principale \" d'un normalien, et la mention qui sera portée sur le diplôme. Le jury peut décider d'une mention différente lorsque le parcours le justifie." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\item[-]  d'une formation complémentaire, dispensée ou agréée par l'ENS, répartie sur trois ans. Le volume global de cette formation doit représenter au moins un volume de travail additionnel équivalent à 1/5 (soit 20\\percent) de la formation principale." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\end{itemize}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\section{Informations sur le titulaire du diplôme}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Nom de famille~:} %s" (Special_char.uppercase dens.Public_data.dens_lastname) in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Prénom~:} %s" (Special_char.uppercase dens.Public_data.dens_firstname) in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Date de naissance~:} %s" (Tools.date_to_string_fr (birthdate dens)) in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Numéro d'identification de l'étudiant (N$^o$ INE)~:} %s" (ine dens) in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\section{Informations sur le diplôme}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Intitulé du diplôme~:} Diplôme de l'École normale supérieure" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Principal(aux) domaine(s) d'étude couvert(s) par le diplôme~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Spécialité principale~: INFORMATIQUE" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Nom et statut de l'établissement ayant délivré le diplôme~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}École normale supérieure (ENS), Paris, France" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Établissement public à caractère scientifique, culturel et professionnel" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Nom et statut de l'établissement dispensant les cours~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}École normale supérieure (ENS), Paris, France" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\mbox{}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Langue(s) utilisée(s) pour l'enseignement et les examens}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Français : 75 \\percent" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Anglais : 25 \\percent" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\section{Informations sur le niveau de qualification}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Conditions d'accès~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Accès par voie de concours (concours CPGE, voie universitaire ou voie de Sélection Internationale), après deux années d'études supérieures minimum" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Durée officielle du programme d'études~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}4 à 6 semestres en fonction de la voie d'accès et de la spécialité choisie" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Niveau de qualification~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Diplôme de grade Master en Sciences (d'un niveau équivalent à un Master à orientation recherche 120 crédits européens compatibles ECTS) auquel s'ajoute un minimum de 36 ECTS d'enseignements complémentaires." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\section{Informations concernant le contenu du diplôme et les résultats obtenus}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Organisation des études~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}La formation est assurée à temps plein avec un stage obligatoire en laboratoire ou en entreprise." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\mbox{}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Exigences du programme~:}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Le Département d'informatique l'ENS assure une formation de haut niveau par la recherche dans le domaine de l'informatique, en mettant l'accent sur des aspects théoriques et fondamentaux de l'informatique moderne. %%\n" in
    let () = Remanent_state.fprintf state "Il prépare plus particulièrement aux métiers de la recherche académique ou industrielle en informatique et dans des disciplines utilisant l'informatique comme outil principal d'investigation. À travers le diplôme de l'ENS (DENS), les étudiants acquièrent en plus de leur spécialité des connaissances dans un ou plusieurs domaines complémentaires. %%\n" in
    let () = Remanent_state.fprintf state "Une importance particulière est accordée à l'interdisciplinarité et aux expériences professionnelles en France et à l'étranger." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Le titulaire du diplôme est un spécialiste dans le domaine de l'informatique. %%\n" in
    let () = Remanent_state.fprintf state "Il maîtrise des techniques de pointe en informatique et en mathématiques appliquées avec la profondeur d'analyse nécessaire pour appréhender les évolutions de sa discipline et y participer. %%\n" in
    let () = Remanent_state.fprintf state "Il est apte à s'intégrer rapidement dans des nouveaux environnements de travail, d'un point de vue aussi bien intellectuel que matériel. %%\n" in
    let () = Remanent_state.fprintf state "Il est autonome, fait preuve d'initiative et assimile rapidement de nouveaux concepts, tout en sachant replacer sa spécialité dans un contexte scientifique plus large. %%\n" in
    let () = Remanent_state.fprintf state "Il sait travailler en équipe et maîtrise la communication scientifique à destination de ses pairs et du grand public." in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "\\clearpage" in
    let () = Remanent_state.print_newline state in
    ()

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

let width_gps_code = 9.
let width_discipline = 6.5
let width_intitule = 20.
let width_ects = 2.5

let width_etbl = 0.7
let width_dom = 1.5
let width_annee = 0.5


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

let size_m2 = compute_size [width_etbl;width_dom;width_annee]
let size4 = compute_size [width_gps_code;width_discipline;width_intitule;width_ects]
let size3 = compute_size [width_gps_code;width_discipline;width_intitule]


let dump_course_gen label is_empty fold fold_state _iter acc state =
    if is_empty acc  then state
    else
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{%s}" label in
    let () = Remanent_state.print_newline state in
    let ects =
          fold
            (fun cours ects -> ects+.cours.Public_data.supplement_ects) acc 0.
    in
    let () = Remanent_state.fprintf state "{\\noindent}Nombre d'ECTS~: %s" (string_of_float  ects) in
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
    let state =
      fold_state
        (fun elt state  ->
            let state, l, _l_en =
              match
                Remanent_state.Translate_courses.get_translation
                  Collect_course_entries.unify_course_entry __POS__
                  elt.Public_data.supplement_intitule
                  state
              with
              | state, (None,None) ->
                state, elt.Public_data.supplement_intitule, elt.Public_data.supplement_intitule
              | state, (Some lib, Some lib') ->
                  state, lib, lib'
              | state, ((Some lib, None) | (None, Some lib)) ->
                  state, lib, lib
          in
          let () = Remanent_state.open_row state in
          let () = Remanent_state.print_cell (elt.Public_data.supplement_code) state in
          let () = Remanent_state.print_cell (elt.Public_data.supplement_discipline) state in
          let () = Remanent_state.print_cell l state in
          let () = Remanent_state.print_cell (string_of_float elt.Public_data.supplement_ects) state in
          let () = Remanent_state.close_row state in
          state)
        acc state
    in
    let () = Remanent_state.close_array state in
    let () = Remanent_state.fprintf state "\\mbox{}\\bigskip" in
    let () = Remanent_state.print_newline state in
    state

    let fold_left_tild f a b = List.fold_left (fun a b -> f b a) b a
    let fold_left_tild' f a b = List.fold_left (fun a b -> f b a) b a
    let dump_course_list label list state =
        let list = list.Public_data.dens in
        dump_course_gen label (fun l -> l = []) fold_left_tild fold_left_tild' List.iter list state

    let dump_course_list_autre label list map state =
        let list = list.Public_data.dens in
        dump_course_gen label
          (fun (list,map) -> list=[] && Public_data.StringMap.is_empty map)
          (fun f (list,map) acc ->
            fold_left_tild f list
              (Public_data.StringMap.fold (fun _  l ->
                    fold_left_tild f l.Public_data.dens) map acc ))
          (fun f (list,map) acc ->
              fold_left_tild' f list
                  (Public_data.StringMap.fold (fun _  l ->
                              fold_left_tild' f l.Public_data.dens) map acc ))
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
            let key = Special_char.lowercase (Dens.string_of_key key) in
            let label = Format.sprintf  "{\\noindent}Enseignements validés dans le cadre d'%s %s en %s" undef label key in
            dump_repartition_diplomes label list state)
        map state
    else
      fst (Public_data.StringMap.fold
        (fun key list (state,pos) ->
            let key = Special_char.lowercase (Dens.string_of_key key) in
          let label = Format.sprintf  "{\\noindent}Enseignements validés dans le cadre d'une %s %s en %s" (List.hd (fst pos)) label key in
          dump_repartition_diplomes label list state,next pos)
      map (state,pos))

let dump_activite_list label list state =
    if list = [] then state
    else
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{%s}" label in
    let () = Remanent_state.print_newline state in
    let ects =
        List.fold_left
          (fun ects cours -> ects+.cours.Public_data.activite_ects) 0. list
    in
    let state,size,bgcolor,title,title_english,ects_ =
        if ects = 0.
        then state,size3,[None;None;None],[["Code"];["Activité"];["Intitulé"]],[["Code"];["Activity"];["Name"]],false
        else
        let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Unités d'enseignement étudiées et nombre d'ECTS}" in
        let () = Remanent_state.print_newline state in
        let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Nombre d'ECTS~: %s}" (string_of_float ects) in
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

let next_year y =
  try
    string_of_int (1+(int_of_string y))
  with
    | _ -> y

let dump_master dens state =
    let list = dens.Public_data.dens_master in
    match list with
        | [] -> state
        | _ ->
          begin
            let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Diplôme national de Master}" in
          let () = Remanent_state.print_newline state in
          let () = Remanent_state.fprintf state "{\\noindent}Nombre d'ECTS~: 120" in
          let () = Remanent_state.print_newline state in
          let size,bgcolor,title,title_english =
              size_m2,[None;None;None],[["Établissement"];["Domaine, Mention, Parcours"];["Année d'obtention"]],[["Etablissement"];["Field, Mention, Track"];["Year of obtention"]]
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
                              (Public_data.string_of_universite_long_fr         elt.Public_data.diplome_univ_key) state in
                    let () = Remanent_state.print_cell (Tools.unsome_string elt.Public_data.diplome_cursus.Public_data.label_sad) state in
                    let () = Remanent_state.print_cell (next_year elt.Public_data.diplome_year) state in
                    let () = Remanent_state.close_row state in
                    ())
                  list
              in
            let () = Remanent_state.close_array state in
            let () = Remanent_state.fprintf state "\\mbox{}\\bigskip" in
            let () = Remanent_state.print_newline state in
            state
          end

let prompt_sad dens state =
    let () = print_preamble state dens in
    let state = dump_master dens state in
    let () = Remanent_state.fprintf state "{\\noindent}\\textbf{Enseignements complémentaires suivis et validés dans le cadre du Diplôme de l'ENS, et ECTS obtenus}" in
    let () = Remanent_state.print_newline state in
    let () = Remanent_state.fprintf state "{\\noindent}Nombre d'ECTS~: %s\\bigskip" (string_of_float  dens.Public_data.dens_total_ects) in
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
    let state = dump_activite_list "Expérience de recherche (collective pour les lettres, de laboratoire pour les sciences)" dens.Public_data.dens_activite_recherche state in
    let state = dump_activite_list "Expérience internationale (stages académiques ou non-académiques à l'étranger)" dens.Public_data.dens_activite_internationale state in
    let state = dump_activite_list "Expérience transdisciplinaire"
    dens.Public_data.dens_activite_transdisciplinaire state in 
    let state =
        dump_activite_list
          "Expérience d'ouverture hors les murs (stages non-académiques uniquement, en France ou à l'étranger: stages en administration, entreprise, lycée, ONG, etc.)" dens.Public_data.dens_activite_ouverture state
    in
    let state =
        dump_course_list
            "Cours de langues étrangères et certifications en langues"
            (lift_dens dens.Public_data.dens_cours_langue) state
    in
    let state = dump_activite_list "Autre" dens.Public_data.dens_activite_autre state in
    let state = dump_course_list "Autres (vie universitaire, initiatives citoyennes, sport, etc.)" (lift_dens dens.Public_data.dens_cours_activite) state
    in
     state




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
      let state, language =
       Tools.get_option
         state
         Remanent_state.get_language
         language
      in
      let ext =
        match language with
        | Public_data.French -> ""
        | Public_data.English -> ".en"
      in
       let output = (repository, Format.sprintf "DENS_%s_%s%s.tex" (Tools.remove_space_from_string lastname) (Tools.remove_space_from_string firstname) ext) in
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
                bilinguage;
              Loggers.font = 9 ;
              Loggers.template = Loggers.SAD;
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
           ~file_name:(Copy.pdf_file (snd output))
           ~output_rep
           state
      end
    else
      state

let dump_sad ~repository?firstname ?lastname ?language ?bilingual state =
  let state, dens_list = Remanent_state.Collector_dens_diplomas.get state in
    List.fold_right
      (dump_one_sad ~repository ?language ?bilingual ?firstname ?lastname)
      dens_list state
