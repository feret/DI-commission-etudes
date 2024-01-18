type t =
  {
    per_promo:
      Public_data.student_pegasus list
        Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.YearMap.t
        Public_data.PromoMap.t ;
    per_name:
      Public_data.student_pegasus list
        Public_data.YearMap.t Public_data.FirstNameMap.t Public_data.LastNameMap.t
  }

let empty =
  {
    per_promo =
      Public_data.PromoMap.empty;
    per_name =
      Public_data.LastNameMap.empty
  }

let get_pegasus_administrative_status ~firstname ~lastname ~year dens_candidates =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in
  let () =
    Format.printf "LOOKING FOR %s %s (%s) @." firstname lastname year
  in
  let () =
    Public_data.LastNameMap.iter (fun x _ -> Format.printf " -> %s" x) dens_candidates.per_name
  in
  match
    Public_data.LastNameMap.find_opt
      lastname
      dens_candidates.per_name
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
          | Some a -> a

let add_pegasus_administrative_status
    unify pos state
    dens_candidate dens_candidates =
  let _ = unify, pos in
  let firstname = dens_candidate.Public_data.pegasus_firstname in
  let lastname = dens_candidate.Public_data.pegasus_lastname in
  let year = dens_candidate.Public_data.pegasus_promotion in
  let dens_candidate_list = get_pegasus_administrative_status ~firstname ~lastname ~year  dens_candidates in
  let dens_candidate_list = dens_candidate::dens_candidate_list in
  let dens_candidates =
    let promo = year in
      let old_promo =
        match
          Public_data.PromoMap.find_opt promo
            dens_candidates.per_promo
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
      let per_promo =
        Public_data.PromoMap.add promo
          (Public_data.YearMap.add year
            (Public_data.LastNameMap.add
              lastname
              (Public_data.FirstNameMap.add
                  firstname
                  dens_candidate_list
                  old_lastname)
               old_year)
          old_promo)
          dens_candidates.per_promo
      in
      {dens_candidates with per_promo}
  in
  let dens_candidates =
    let old_lastname =
      match
        Public_data.LastNameMap.find_opt
          lastname
          dens_candidates.per_name
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
    let per_name =
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname
           (Public_data.YearMap.add
              year
              dens_candidate_list
              old_firstname)
           old_lastname)
        dens_candidates.per_name
    in
    {dens_candidates with per_name}
  in
  state, dens_candidates
