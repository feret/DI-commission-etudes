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

let unify ~safe_mode logger prefix pos error mentoring mentoring' =
  if
    Special_char.correct_string mentoring.Public_data.nom_de_l_etudiant
    =
    Special_char.correct_string mentoring'.Public_data.nom_de_l_etudiant
    &&
    Special_char.correct_string mentoring.Public_data.prenom_de_l_etudiant
    =
    Special_char.correct_string mentoring'.Public_data.prenom_de_l_etudiant
    &&
    mentoring.Public_data.annee_academique =
    mentoring'.Public_data.annee_academique
  then
    let error,mentoring =
      match mentoring.Public_data.nom_du_tuteur,
            mentoring'.Public_data.nom_du_tuteur
      with
      | _, None -> error,mentoring
      | None, _ -> error,
                   {mentoring
                    with Public_data.nom_du_tuteur = mentoring'.Public_data.nom_du_tuteur}
      | Some a, Some a' when Special_char.correct_string a = Special_char.correct_string a' -> error,mentoring
      | Some a, Some a' ->
          let message =
            Format.sprintf
              "Incompatible tuteur's last names  for %s %s's mentoging in %s  (%s VS %s)"
              mentoring.Public_data.prenom_de_l_etudiant
              mentoring.Public_data.nom_de_l_etudiant
              mentoring.Public_data.annee_academique
              a a'
          in
          Exception.warn
            logger
            ~safe_mode
            ~message
            prefix
            error
            pos
            Exit
            mentoring
    in
    let error,mentoring =
      match mentoring.Public_data.prenom_du_tuteur,
            mentoring'.Public_data.prenom_du_tuteur
      with
      | _, None -> error,mentoring
      | None, _ -> error,
                   {mentoring
                    with Public_data.prenom_du_tuteur = mentoring'.Public_data.prenom_du_tuteur}
      | Some a, Some a' when Special_char.correct_string a = Special_char.correct_string a' -> error,mentoring
      | Some a, Some a' ->
          let message =
            Format.sprintf
              "Incompatible tuteur's first names  for %s %s's mentoging in %s  (%s VS %s)"
              mentoring.Public_data.prenom_de_l_etudiant
              mentoring.Public_data.nom_de_l_etudiant
              mentoring.Public_data.annee_academique
              a a'
          in
          Exception.warn
            logger
            ~safe_mode
            ~message
            prefix
            error
            pos
            Exit
            mentoring
    in
    let error,mentoring =
      match mentoring.Public_data.courriel_du_tuteur,
            mentoring'.Public_data.courriel_du_tuteur
      with
      | _, None -> error,mentoring
      | None, _ -> error,
                   {mentoring
                    with Public_data.courriel_du_tuteur = mentoring'.Public_data.courriel_du_tuteur}
      | Some a, Some a' when Special_char.correct_string a = Special_char.correct_string a' -> error,mentoring
      | Some a, Some a' ->
          let message =
            Format.sprintf
              "Incompatible tuteur's email address  for %s %s's mentoging in %s  (%s VS %s)"
              mentoring.Public_data.prenom_de_l_etudiant
              mentoring.Public_data.nom_de_l_etudiant
              mentoring.Public_data.annee_academique
              a a'
          in
          Exception.warn
            logger
            ~safe_mode
            ~message
            prefix
            error
            pos
            Exit
            mentoring
    in
    error, mentoring
  else
  let message =
    Format.sprintf
      "Cannot unify mentoring data with different names and academic years %s %s (%s) VS %s %s (%s)"
      mentoring.Public_data.prenom_de_l_etudiant
      mentoring.Public_data.nom_de_l_etudiant
      mentoring.Public_data.annee_academique
      mentoring'.Public_data.prenom_de_l_etudiant
      mentoring'.Public_data.nom_de_l_etudiant
      mentoring'.Public_data.annee_academique
  in
  Exception.warn
    logger
    ~safe_mode
    ~message
    prefix
    error
    pos
    Exit
    mentoring

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
    ~safe_mode logger prefix pos error
    tutorat mentoring =
  let firstname = tutorat.Public_data.prenom_de_l_etudiant in
  let lastname = tutorat.Public_data.nom_de_l_etudiant in
  let year = tutorat.Public_data.annee_academique in
  let tutorat' =
    get_mentoring ~strong:true ~firstname ~lastname ~year mentoring
  in
  let error, tutorat =
    match tutorat' with
    | None -> error, tutorat
    | Some b ->
      unify ~safe_mode logger prefix pos error
        tutorat b
  in
  (*  per_mentor:
    Public_data.tutorat Public_data.YearMap.t
      Public_data.FirstNameMap.t Public_data.LastNameMap.t;*)

  let mentoring =
    let per_year =
      let old_year =
        match
          Public_data.YearMap.find_opt
            tutorat.Public_data.annee_academique
            mentoring.per_year
        with
        | Some map -> map
        | None -> Public_data.LastNameMap.empty
      in
      let old_lastname =
        match
          Public_data.LastNameMap.find_opt
            tutorat.Public_data.nom_de_l_etudiant
            old_year
        with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
      in
      Public_data.YearMap.add
        tutorat.Public_data.annee_academique
        (Public_data.LastNameMap.add
           tutorat.Public_data.nom_de_l_etudiant
           (Public_data.FirstNameMap.add
              tutorat.Public_data.prenom_de_l_etudiant
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
            tutorat.Public_data.nom_de_l_etudiant
            mentoring.per_name
        with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
      in
      let old_firstname =
        match
          Public_data.FirstNameMap.find_opt
            tutorat.Public_data.prenom_de_l_etudiant
            old_name
        with
        | Some map -> map
        | None -> Public_data.YearMap.empty
      in
      Public_data.LastNameMap.add
        tutorat.Public_data.nom_de_l_etudiant
        (Public_data.FirstNameMap.add
           tutorat.Public_data.prenom_de_l_etudiant
           (Public_data.YearMap.add
              tutorat.Public_data.annee_academique
              tutorat
              old_firstname)
           old_name)
        mentoring.per_name
    in
    {mentoring  with per_name }
  in
  let mentoring =
    match tutorat.Public_data.nom_du_tuteur,
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
            tutorat.Public_data.annee_academique
            old_mentor_firstname
        with
        | Some map -> map
        | None -> Public_data.LastNameMap.empty
      in
      let old_lastname =
        match
          Public_data.LastNameMap.find_opt
            tutorat.Public_data.nom_de_l_etudiant
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
             tutorat.Public_data.annee_academique
             (Public_data.LastNameMap.add
                tutorat.Public_data.nom_de_l_etudiant
                (Public_data.FirstNameMap.add
                   tutorat.Public_data.prenom_de_l_etudiant
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

  error, mentoring

let get_mentoring = get_mentoring ~strong:false
