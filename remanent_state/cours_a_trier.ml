type t =
    Public_data.cours_a_trier
      Public_data.CodeExtendedMap.t
      Public_data.LibelleExtendedMap.t
      Public_data.YearExtendedMap.t
      Public_data.FirstNameExtendedMap.t
      Public_data.LastNameExtendedMap.t

let empty = Public_data.LastNameMap.empty

let fold_left_rev f a b = List.fold_left (fun a b -> f b a) b a

let get_sorted_courses ?firstname ?lastname ?year ?libelle ?codegps
    t =
    let codegps =
      Tools.map_opt Special_char.lowercase codegps
    in
    let firstname =
        Tools.map_opt String.lowercase_ascii firstname
    in
    let lastname =
        Tools.map_opt String.lowercase_ascii lastname
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
    let acc3 =
        fold_left_rev
             (Public_data.LibelleExtendedMap.collect libelle)
             acc2
             []
    in
    fold_left_rev
         (Public_data.CodeExtendedMap.collect codegps)
         acc3
         []

let get_sorted_course ~firstname ~lastname ~year ~libelle ~codegps (t:t) =
    let codegps =
        Special_char.lowercase codegps
    in
    let firstname =
        String.lowercase_ascii firstname
    in
    let lastname =
        String.lowercase_ascii lastname
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
                match Public_data.LibelleMap.find_opt libelle map with
                  | None -> None
                  | Some map ->
                  match Public_data.CodeMap.find_opt codegps map with
                    | None -> None
                    | Some map -> Some map

  let add_sorted_course
      unify pos state
      cours_a_trier liste_cours_a_trier  =
    let firstname = cours_a_trier.Public_data.coursat_prenom in
    let lastname = cours_a_trier.Public_data.coursat_nom in
    let year = cours_a_trier.Public_data.coursat_annee in
    let libelle = cours_a_trier.Public_data.coursat_libelle in
    let codegps = cours_a_trier.Public_data.coursat_codegps in
    let cours_a_trier_list =
            get_sorted_course
                    ~firstname ~lastname ~year ~libelle ~codegps
                                liste_cours_a_trier in
    let state, cours_a_trier_list =
        match cours_a_trier_list
        with
          | None -> state, cours_a_trier
          | Some a ->
              let state, cours_a_trier = unify pos state cours_a_trier a in
                  state, cours_a_trier
    in
    let old_lastname =
      match
        Public_data.LastNameMap.find_opt
            lastname
            liste_cours_a_trier
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
    let old_libelle =
      match
        Public_data.LibelleMap.find_opt
          libelle
          old_year
      with
      | Some map -> map
      | None -> Public_data.CodeMap.empty
    in
      let (liste_cours_a_trier:t) =
      Public_data.LastNameMap.add lastname
            (Public_data.FirstNameMap.add firstname
                    (Public_data.YearMap.add year
                        (Public_data.LibelleMap.add libelle
                            (Public_data.CodeMap.add codegps
                                cours_a_trier_list
                                old_libelle)
                        old_year)
                  old_firstname)
            old_lastname)
        liste_cours_a_trier
    in
    state, liste_cours_a_trier
