type t =
  {
    per_promo:
      Public_data.dens_candidate list
        Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.PromoMap.t ;
    per_name:
      Public_data.dens_candidate list
        Public_data.FirstNameMap.t Public_data.LastNameMap.t
  }

let empty =
  {
    per_promo =
      Public_data.PromoMap.empty;
    per_name =
      Public_data.LastNameMap.empty
  }

let get_dens_candidate ~firstname ~lastname dens_candidates =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
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
    | Some a -> a
    | None -> []

let add_dens_candidate
    unify pos state
    dens_candidate dens_candidates =
  let _ = unify, pos in
  let firstname = dens_candidate.Public_data.dens_candidate_firstname in
  let lastname = dens_candidate.Public_data.dens_candidate_lastname in
  let dens_candidate_list = get_dens_candidate ~firstname ~lastname dens_candidates in
  let dens_candidate_list = dens_candidate::dens_candidate_list in
  let dens_candidates =
    let promo = dens_candidate.Public_data.dens_candidate_promotion in
      let old_promo =
        match
          Public_data.PromoMap.find_opt promo
            dens_candidates.per_promo
        with
        | Some map -> map
        | None -> Public_data.LastNameMap.empty
      in
      let old_lastname =
        match
          Public_data.LastNameMap.find_opt
            lastname
            old_promo
        with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
      in
      let per_promo =
        Public_data.PromoMap.add promo
          (Public_data.LastNameMap.add
             lastname
             (Public_data.FirstNameMap.add
                firstname dens_candidate_list
                old_lastname)
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
    let per_name =
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname dens_candidate_list
           old_lastname)
        dens_candidates.per_name
    in
    {dens_candidates with per_name}
  in
  state, dens_candidates
