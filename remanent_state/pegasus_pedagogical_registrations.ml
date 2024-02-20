type t =
  {
    per_promo:
      Public_data.pedagogical_entry_pegasus list
        Public_data.FirstNameMap.t Public_data.LastNameMap.t     Public_data.PromoMap.t ;
    per_name:
      Public_data.pedagogical_entry_pegasus list
        Public_data.PromoMap.t Public_data.FirstNameMap.t Public_data.LastNameMap.t
  }

let empty =
  {
    per_promo =
      Public_data.PromoMap.empty;
    per_name =
      Public_data.LastNameMap.empty
  }

let get_pegasus_pedagocial_registrations ~firstname ~lastname ~year dens_candidates =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in
  (*let () =
    Format.printf "LOOKING FOR %s %s (%s) @." firstname lastname year
  in*)
  (*let () =
    Public_data.LastNameMap.iter (fun x _ -> Format.printf " -> %s @." x) dens_candidates.per_name
  in*)
  let l =
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
          Public_data.PromoMap.find_opt
            year a
        with None -> []
          | Some a -> a
  in
  let () =
    Format.printf "LOOKING FOR %s %s (%s) %i @." firstname lastname year (List.length l)
  in
  l
  (*let () =
    Public_data.LastNameMap.iter (fun x _ -> Format.printf " -> %s @." x) dens_candidates.per_name
  in*)


          let dump m =
            let () = Format.printf "DUMP PEGASUS CONTENT @." in Public_data.LastNameMap.iter
              (fun x map ->
                  Public_data.FirstNameMap.iter
                    (fun y map ->
                        Public_data.PromoMap.iter
                          (fun z t ->
                              let () = Format.printf "%s %s (%s) (PEGASUS CONTENT) %i @." x y z (List.length t) in ())

                          map
                    ) map
                ) m.per_name

let add_pegasus_pedagocial_registrations
    unify pos state
    dens_candidate dens_candidates =
  let _ = unify, pos in
  let firstname = dens_candidate.Public_data.pe_firstname in
  let lastname = dens_candidate.Public_data.pe_lastname in
  let year = dens_candidate.Public_data.pe_promotion in
  (*let () = Format.printf "%s %s (%s) (ADD PEGAGUS)" firstname lastname year in*)
  let dens_candidate_list = get_pegasus_pedagocial_registrations ~firstname ~lastname  ~year  dens_candidates in
  let dens_candidate_list = dens_candidate::dens_candidate_list in
  let dens_candidates =
    let promo = year in
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
                  firstname
                  dens_candidate_list
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
    let old_firstname =
      match
        Public_data.FirstNameMap.find_opt
          firstname
          old_lastname
      with
      | Some map -> map
      | None -> Public_data.PromoMap.empty
    in
    let per_name =
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname
           (Public_data.PromoMap.add
              year
              dens_candidate_list
              old_firstname)
           old_lastname)
        dens_candidates.per_name
    in
    {dens_candidates with per_name}
  in
  state, dens_candidates
