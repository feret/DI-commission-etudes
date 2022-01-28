type t =
  {
    per_promo:
      Public_data.scholarship list
        Public_data.FirstNameMap.t Public_data.LastNameMap.t Public_data.PromoMap.t ;
    per_name:
      Public_data.scholarship list
        Public_data.FirstNameMap.t Public_data.LastNameMap.t
  }

let empty =
  {
    per_promo =
      Public_data.PromoMap.empty;
    per_name =
      Public_data.LastNameMap.empty
  }

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
  | None -> []
  | Some a ->
    match
      Public_data.FirstNameMap.find_opt
        firstname
        a
    with
    | Some a -> a
    | None -> []

let add_scholarship
    unify pos state
    boursier scholarships =
  let _ = unify, pos in 
  let firstname = boursier.Public_data.holder_firstname in
  let lastname = boursier.Public_data.holder_lastname in
  let boursier_list = get_scholarship ~firstname ~lastname scholarships in
  let boursier_list = boursier::boursier_list in
  let scholarships  =
    match boursier.Public_data.holder_promotion with
    | None -> scholarships
    | Some promo ->
      let old_promo =
        match
          Public_data.PromoMap.find_opt promo
            scholarships.per_promo
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
                firstname boursier_list
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
          lastname
          scholarships.per_name
      with
      | Some map -> map
      | None -> Public_data.FirstNameMap.empty
    in
    let per_name =
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname boursier_list
           old_lastname)
        scholarships.per_name
    in
    {scholarships with per_name}
  in
  state, scholarships
