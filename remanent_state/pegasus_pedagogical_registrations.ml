type t =
      Public_data.pedagogical_entry_pegasus list
        Public_data.FirstNameMap.t Public_data.LastNameMap.t

let empty =
      Public_data.LastNameMap.empty

let get_pegasus_pedagocial_registrations ~firstname ~lastname  dens_candidates =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in
  let l =
  match
    Public_data.LastNameMap.find_opt
      lastname
      dens_candidates
  with
  | None -> []
  | Some a ->
    match
      Public_data.FirstNameMap.find_opt
        firstname
        a
    with
    | None -> []
    | Some a -> a
  in
  let () =
    Format.printf "LOOKING FOR %s %s %i @." firstname lastname  (List.length l)
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

                              let () = Format.printf "%s %s (PEGASUS CONTENT) %i @." x y (List.length map) in ())


                     map
                ) m

let add_pegasus_pedagocial_registrations
    unify pos state
    dens_candidate dens_candidates =
  let _ = unify, pos in
  let firstname = dens_candidate.Public_data.pe_firstname in
  let lastname = dens_candidate.Public_data.pe_lastname in
  (*let () = Format.printf "%s %s (%s) (ADD PEGAGUS)" firstname lastname year in*)
  let dens_candidate_list = get_pegasus_pedagocial_registrations ~firstname ~lastname   dens_candidates in
  let dens_candidate_list = dens_candidate::dens_candidate_list in
    let dens_candidates =
    let old_lastname =
      match
        Public_data.LastNameMap.find_opt
          lastname
          dens_candidates
      with
      | Some map -> map
      | None -> Public_data.FirstNameMap.empty
    in

    let per_name =
      Public_data.LastNameMap.add
        lastname
        (Public_data.FirstNameMap.add
           firstname
            dens_candidate_list
           old_lastname)
        dens_candidates
    in
    per_name
  in
  state, dens_candidates
