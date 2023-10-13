type t =
  {
    per_promo:
      Public_data.mineure_majeure list
        Public_data.DptMap.t
        Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.YearMap.t
        Public_data.PromoMap.t ;
    per_name:
      Public_data.mineure_majeure list
        Public_data.DptMap.t
        Public_data.YearMap.t
        Public_data.FirstNameMap.t Public_data.LastNameMap.t
  }

let empty =
  {
    per_promo =
      Public_data.PromoMap.empty;
    per_name =
      Public_data.LastNameMap.empty
  }

let get_minor_candidate ~firstname ~lastname ~year ?dpt minor_candidates =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in
  match
    Public_data.LastNameMap.find_opt
      lastname
      minor_candidates.per_name
  with
  | None -> []
  | Some a ->
    match
      Public_data.FirstNameMap.find_opt
        firstname
        a
    with
    | None -> []
    | Some a ->
        match
          Public_data.YearMap.find_opt
            year a
        with None -> []
          | Some a ->
            match dpt with
            | Some dpt ->
                begin
                  match
                    Public_data.DptMap.find_opt
                      dpt a
                  with None -> []
                    | Some a -> a
                end
          | None ->
              Public_data.DptMap.fold
                (fun _ l l' ->  l@l')
                a []

let add_minor_candidate
    unify pos state
    minor_candidate minor_candidates =
  let firstname = minor_candidate.Public_data.secondary_student_firstname in
  let lastname = minor_candidate.Public_data.secondary_student_lastname in
  let year = minor_candidate.Public_data.secondary_diplomation_year in
  let dpt  = minor_candidate.Public_data.secondary_dpt in
  let minor_candidate_list = get_minor_candidate ~firstname ~lastname ~year ~dpt minor_candidates in
  let state, minor_candidate_list =
      match minor_candidate_list with [] -> state, [minor_candidate]
  | a::b -> let state, minor_candidate = unify pos state minor_candidate a in
            state, minor_candidate::b
  in
  let minor_candidates =
    let promo = minor_candidate.Public_data.secondary_student_promo in
      let old_promo =
        match
          Public_data.PromoMap.find_opt promo
            minor_candidates.per_promo
        with
        | Some map -> map
        | None -> Public_data.YearMap.empty
      in
      let old_year =
        match
          Public_data.YearMap.find_opt year
            old_promo
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
      let old_firstname =
      match
        Public_data.FirstNameMap.find_opt
          firstname
          old_lastname
      with
      | Some map -> map
      | None -> Public_data.DptMap.empty
    in
      let per_promo =
        Public_data.PromoMap.add promo
          (Public_data.YearMap.add year
            (Public_data.LastNameMap.add
              lastname
              (Public_data.FirstNameMap.add
                  firstname
                  (Public_data.DptMap.add
                      dpt minor_candidate_list
                  old_firstname)
              old_lastname)
            old_year)
          old_promo)
          minor_candidates.per_promo
      in
      {minor_candidates with per_promo}
  in
  let minor_candidates =
    let old_lastname =
      match
        Public_data.LastNameMap.find_opt
          lastname
          minor_candidates.per_name
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
        | None -> Public_data.DptMap.empty
    in
    let per_name =
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname
           (Public_data.YearMap.add
              year
              (Public_data.DptMap.add dpt
              minor_candidate_list old_year)
              old_firstname)
           old_lastname)
        minor_candidates.per_name
    in
    {minor_candidates with per_name}
  in
  state, minor_candidates
