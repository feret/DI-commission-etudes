type t =
  {
    per_year:
      Public_data.tutorat Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.YearMap.t ;
    per_name:
      Public_data.tutorat Public_data.YearMap.t
        Public_data.FirstNameMap.t Public_data.LastNameMap.t;
    per_mentor:
      Public_data.tutorat
        Public_data.FirstNameMap.t
        Public_data.LastNameMap.t
        Public_data.YearMap.t
        Public_data.FirstNameMap.t
        Public_data.LastNameMap.t;
  }

let empty =
  {
    per_year =
      Public_data.YearMap.empty;
    per_name =
      Public_data.LastNameMap.empty;
    per_mentor =
      Public_data.LastNameMap.empty
  }

let get_mentoring ~strong ~year ~firstname ~lastname mentoring =
  let firstname =
    Special_char.lowercase firstname
  in
  let lastname =
    Special_char.lowercase lastname
  in
  match
    Public_data.LastNameMap.find_opt
      lastname
      mentoring.per_name
  with
  | None -> None
  | Some a ->
    let yearmap =
      (match
         Public_data.FirstNameMap.find_opt
           firstname
           a
       with
       | None -> Public_data.YearMap.empty
       | Some a -> a)
    in
    if strong
    then
      Public_data.YearMap.find_opt
        year yearmap
    else
      Public_data.YearMap.fold
        (fun year' mentoring output ->
           if compare year' year <= 0
           then Some mentoring
           else output)
        yearmap None


let add_mentoring
    unify pos state
    tutorat mentoring =
  let firstname = tutorat.Public_data.prenom_de_l_etudiant in
  let lastname = tutorat.Public_data.nom_de_l_etudiant in
  let year = tutorat.Public_data.annee_academique in
  let tutorat_opt' =
    get_mentoring ~strong:true ~firstname ~lastname ~year mentoring
  in
  let state, tutorat =
    match tutorat_opt' with
    | None -> state, tutorat
    | Some tutorat' ->
        unify pos state tutorat tutorat'
  in
  let mentoring =
    let per_year =
      let old_year =
        match
          Public_data.YearMap.find_opt
            year
            mentoring.per_year
        with
        | Some map -> map
        | None -> Public_data.LastNameMap.empty
      in
      let old_lastname =
        match
          Public_data.LastNameMap.find_opt
            lastname
            old_year
        with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
      in
      Public_data.YearMap.add
        year
        (Public_data.LastNameMap.add
           lastname
           (Public_data.FirstNameMap.add
              firstname
              tutorat
              old_lastname)
           old_year)
        mentoring.per_year
    in
    {mentoring  with per_year}
  in
  let mentoring =
    let per_name =
      let old_name =
        match
          Public_data.LastNameMap.find_opt
            lastname
            mentoring.per_name
        with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
      in
      let old_firstname =
        match
          Public_data.FirstNameMap.find_opt
            firstname
            old_name
        with
        | Some map -> map
        | None -> Public_data.YearMap.empty
      in
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname
           (Public_data.YearMap.add
              year
              tutorat
              old_firstname)
           old_name)
        mentoring.per_name
    in
    {mentoring  with per_name }
  in
  let mentoring =
    match
      tutorat.Public_data.nom_du_tuteur,
      tutorat.Public_data.prenom_du_tuteur
    with
    | Some nom_du_tuteur, Some prenom_du_tuteur
      ->
      let per_mentor =
        let old_mentor_name =
          match
            Public_data.LastNameMap.find_opt
              nom_du_tuteur
              mentoring.per_mentor
          with
          | Some map -> map
          | None -> Public_data.FirstNameMap.empty
        in
        let old_mentor_firstname=
          match
            Public_data.FirstNameMap.find_opt
              prenom_du_tuteur
              old_mentor_name
          with
          | Some map -> map
          | None -> Public_data.YearMap.empty
        in
        let old_year =
          match
            Public_data.YearMap.find_opt
              year
              old_mentor_firstname
          with
          | Some map -> map
          | None -> Public_data.LastNameMap.empty
        in
        let old_lastname =
          match
            Public_data.LastNameMap.find_opt
              lastname
              old_year
          with
          | Some map -> map
          | None -> Public_data.FirstNameMap.empty
        in
        Public_data.LastNameMap.add
          nom_du_tuteur
          (Public_data.FirstNameMap.add
             prenom_du_tuteur
             (Public_data.YearMap.add
                year
                (Public_data.LastNameMap.add
                   lastname
                   (Public_data.FirstNameMap.add
                      firstname
                      tutorat
                      old_lastname)
                   old_year)
                old_mentor_firstname)
             old_mentor_name)
          mentoring.per_mentor
      in
      {mentoring  with per_mentor }
    | _ -> mentoring
  in
  state, mentoring

let get_mentoring = get_mentoring ~strong:false

let fold_left_rev f a b = List.fold_left (fun a b -> f b a) b a
let get_mentoring_list
    ?year ?tuteur_lastname ?tuteur_firstname
    t =
  match year with
  | Some _ ->
    let acc1 =
      Public_data.YearExtendedMap.collect
        year
        t.per_year
        []
    in
    let acc2 =
      fold_left_rev
        (Public_data.LastNameExtendedMap.collect
           tuteur_lastname)
        acc1
        []
    in
    fold_left_rev
      (Public_data.FirstNameExtendedMap.collect
         tuteur_firstname)
      acc2
      []
  | None ->
    let acc1 =
      Public_data.LastNameExtendedMap.collect
        tuteur_lastname
        t.per_mentor
        []
    in
    let acc2 =
      fold_left_rev
        (Public_data.FirstNameExtendedMap.collect
           tuteur_firstname)
        acc1
        []
    in
    let acc3 =
      fold_left_rev
        (Public_data.YearExtendedMap.collect
           None)
        acc2
        []
    in
    let acc4 =
      fold_left_rev
        (Public_data.LastNameExtendedMap.collect
           None)
        acc3
        []
    in
    fold_left_rev
      (Public_data.FirstNameExtendedMap.collect
         None)
      acc4
      []
