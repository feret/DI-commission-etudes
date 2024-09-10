type t =
      Public_data.stage_pegasus list
          Public_data.FirstNameMap.t
            Public_data.LastNameMap.t

let empty = Public_data.LastNameMap.empty


let get_pegasus_stages ~firstname ~lastname stages =
  let firstname =
    String.lowercase_ascii firstname
  in
  let lastname =
    String.lowercase_ascii lastname
  in
  match
    Public_data.LastNameMap.find_opt lastname stages
  with
    | None -> []
    | Some c ->
        match Public_data.FirstNameMap.find_opt firstname c with
          | None -> []
          | Some c -> c 



let add_pegasus_stage
    unify pos state
    stage stages =
  let _ = unify, pos in
  let firstname = stage.Public_data.pegasus_stage_firstname in
  let lastname = stage.Public_data.pegasus_stage_lastname in
  let stage_list = get_pegasus_stages ~firstname ~lastname stages in
  let stage_list = stage::stage_list in
  let stages  =
      let old_lastname =
        match
          Public_data.LastNameMap.find_opt
            lastname
            stages
        with
        | Some map -> map
        | None -> Public_data.FirstNameMap.empty
      in
      Public_data.LastNameMap.add
          lastname
          (Public_data.FirstNameMap.add
                firstname stage_list old_lastname)
          stages
  in
  state, stages

let dump  =
      Public_data.LastNameMap.iter
        (fun lastname ->
            Public_data.FirstNameMap.iter
                (fun firstname  ->
                    List.iter
                        (fun stage ->
                          Format.printf
                            "%s %s %s %s %s %s %s %s @."
                            lastname firstname
                            (Tools.unsome_float  stage.Public_data.pegasus_stage_credits)
                            (match  stage.Public_data.pegasus_stage_valide with
                              | None -> ""
                              | Some true -> "O"
                              | Some false -> "F")
                            (Tools.unsome_string  stage.Public_data.pegasus_stage_commentaire)
                        (Tools.unsome_string  stage.Public_data.pegasus_stage_periode)
                        (Tools.unsome_string  stage.Public_data.pegasus_stage_sujet)
                        (Tools.unsome_string  stage.Public_data.pegasus_stage_directeur))))
