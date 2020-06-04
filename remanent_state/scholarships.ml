type t =
  {
    per_promo:
      Public_data.scholarship Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.PromoMap.t ;
    per_name:
      Public_data.scholarship
        Public_data.FirstNameMap.t Public_data.LastNameMap.t
  }

let empty =
  {
    per_promo =
      Public_data.PromoMap.empty;
    per_name =
      Public_data.LastNameMap.empty
  }

let unify ~safe_mode logger prefix pos error boursier boursier' =
  if
    boursier.Public_data.organism = boursier'.Public_data.organism
  then
    if
    boursier.Public_data.holder_lastname = boursier'.Public_data.holder_lastname
    then
      if
        boursier.Public_data.holder_firstname =
        boursier'.Public_data.holder_firstname
      then
        match boursier.Public_data.holder_promotion, boursier'.Public_data.holder_promotion
        with
        | _, None -> error, boursier
        | None, _ -> error, boursier'
        | Some a, Some a' when a=a' -> error, boursier
        | Some a, Some a' ->
          let message =
            Format.sprintf
              "Incompatible promotions for %s %s's scholarship (%s VS %s)"
              boursier.Public_data.holder_firstname
              boursier.Public_data.holder_lastname
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
            boursier
      else
        let message =
          Format.sprintf
            "Incompatible first names to unify the %s's scholarship  (%s VS %s)"
            boursier.Public_data.holder_lastname
            boursier.Public_data.holder_firstname
            boursier'.Public_data.holder_firstname
        in
        Exception.warn
          logger
          ~safe_mode
          ~message
          prefix
          error
          pos
          Exit
          boursier
    else
    let message =
      Format.sprintf
        "Incompatible names to unify a scholarship (%s VS %s)"
        boursier.Public_data.holder_lastname
        boursier'.Public_data.holder_lastname
    in
    Exception.warn
      logger
      ~safe_mode
      ~message
      prefix
      error
      pos
      Exit
      boursier
  else
    let message =
    Format.sprintf
      "Incompatible organismes to unify %s %s's scholarship (%s VS %s)"
      boursier.Public_data.holder_firstname
      boursier.Public_data.holder_lastname
      boursier.Public_data.organism
      boursier'.Public_data.organism
    in
    Exception.warn
      logger
      ~safe_mode
      ~message
      prefix
      error
      pos
      Exit
      boursier

let get_scholarship ~firstname ~lastname scholarships =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in 
  match
    Public_data.LastNameMap.find_opt
      lastname
      scholarships.per_name
  with
  | None -> None
  | Some a ->
    Public_data.FirstNameMap.find_opt
      firstname
      a

let add_scholarship
    ~safe_mode logger prefix pos error
    boursier scholarships =
  let firstname = boursier.Public_data.holder_firstname in
  let lastname = boursier.Public_data.holder_lastname in
  let boursier' = get_scholarship ~firstname ~lastname scholarships in
  let error, boursier =
    match boursier' with
    | None -> error, boursier
    | Some b ->
      unify ~safe_mode logger prefix pos error
        boursier b
  in
  let scholarships  =
    match boursier.Public_data.holder_promotion with
    | None -> scholarships
    | Some promo ->
      let old_promo =
        match
          Public_data.PromoMap.find_opt promo scholarships.per_promo
        with
        | Some map -> map
        | None -> Public_data.LastNameMap.empty
      in
      let old_lastname =
        match
          Public_data.LastNameMap.find_opt
            boursier.Public_data.holder_lastname
            old_promo
        with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
      in
      let per_promo =
        Public_data.PromoMap.add promo
          (Public_data.LastNameMap.add
             boursier.Public_data.holder_lastname
             (Public_data.FirstNameMap.add
                boursier.Public_data.holder_firstname boursier
                old_lastname)
             old_promo)
          scholarships.per_promo

      in
      {scholarships with per_promo}
  in
  let scholarships =
    let old_lastname =
      match
        Public_data.LastNameMap.find_opt
          boursier.Public_data.holder_lastname scholarships.per_name
      with
      | Some map -> map
      | None -> Public_data.FirstNameMap.empty
    in
    let per_name =
      Public_data.LastNameMap.add
        boursier.Public_data.holder_lastname
        (Public_data.FirstNameMap.add
           boursier.Public_data.holder_firstname boursier old_lastname)
        scholarships.per_name
    in
    {scholarships with per_name}
  in
  error, scholarships
