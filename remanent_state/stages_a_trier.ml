type t =
    Public_data.stage_a_trier
      Public_data.LibelleExtendedMap.t
      Public_data.YearExtendedMap.t
      Public_data.FirstNameExtendedMap.t
      Public_data.LastNameExtendedMap.t

let empty = Public_data.LastNameMap.empty

let fold_left_rev f a b = List.fold_left (fun a b -> f b a) b a

let get_sorted_internships ?firstname ?lastname ?year ?libelle
    t =
    let firstname =
        Tools.map_opt String.lowercase_ascii firstname
    in
    let lastname =
        Tools.map_opt String.lowercase_ascii lastname
    in
    let libelle =
        Tools.map_opt String.lowercase_ascii libelle
    in
    let acc0 =
        Public_data.LastNameExtendedMap.collect
            lastname
            t
            []
    in
    let acc1 =
        fold_left_rev
             (Public_data.FirstNameExtendedMap.collect firstname)
             acc0
            []
    in
    let acc2 =
        fold_left_rev
              (Public_data.YearExtendedMap.collect year)
              acc1
              []
    in
    fold_left_rev
        (Public_data.LibelleExtendedMap.collect libelle)
        acc2
        []

let get_sorted_internship ~firstname ~lastname ~year ~libelle t =
    let firstname =
        String.lowercase_ascii firstname
    in
    let lastname =
        String.lowercase_ascii lastname
    in
    let libelle =
        Tools.map_opt String.lowercase_ascii libelle
    in 
    match Public_data.LastNameMap.find_opt lastname t with
      | None -> None
      | Some map ->
          match Public_data.FirstNameMap.find_opt firstname map with
            | None -> None
            | Some map ->
              match Public_data.YearMap.find_opt year map with
                | None -> None
                | Some map ->
                Public_data.LibelleMap.find_opt libelle map


  let add_sorted_internship
      unify pos state
      stage_a_trier liste_stages_a_trier  =
    let firstname = stage_a_trier.Public_data.stageat_prenom in
    let lastname = stage_a_trier.Public_data.stageat_nom in
    let year = stage_a_trier.Public_data.stageat_annee in
    let libelle = stage_a_trier.Public_data.stageat_libelle in
    let stages_a_trier_list =
            get_sorted_internship
                    ~firstname ~lastname ~year ~libelle
                                liste_stages_a_trier in
    let state, stages_a_trier_list =
        match stages_a_trier_list
        with
          | None -> state, stage_a_trier
          | Some a ->
              let state, stage_a_trier = unify pos state stage_a_trier a in
                  state, stage_a_trier
    in
    let old_lastname =
      match
        Public_data.LastNameMap.find_opt
            lastname
            liste_stages_a_trier
      with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
    in
    let old_firstname =
      match
        Public_data.FirstNameMap.find_opt
            firstname
            old_lastname
      with
        | Some map -> map
        | None -> Public_data.YearMap.empty
    in
    let old_year =
      match
        Public_data.YearMap.find_opt
          year
          old_firstname
      with
      | Some map -> map
      | None -> Public_data.LibelleMap.empty
    in
      let liste_stages_a_trier =
      Public_data.LastNameMap.add lastname
            (Public_data.FirstNameMap.add firstname
                    (Public_data.YearMap.add year
                        (Public_data.LibelleMap.add libelle
                                stages_a_trier_list
                        old_year)
                  old_firstname)
            old_lastname)
        liste_stages_a_trier
    in
    state, liste_stages_a_trier
