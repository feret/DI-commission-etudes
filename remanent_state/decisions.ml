type t =
  Public_data.decision
    Public_data.AcronymMap.t
    Public_data.ProgramMap.t
    Public_data.YearMap.t
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

let empty = Public_data.LastNameMap.empty

let fold_left_rev f a b = List.fold_left (fun a b -> f b a) b a

let get_decision ~firstname ~lastname ?year ?program ?dpt
    (decisions:t) =
  match
    Public_data.LastNameMap.find_opt
      lastname
      decisions
  with
  | None -> []
  | Some map
    ->
    match
      Public_data.FirstNameMap.find_opt
        firstname
        map
    with
    | None -> []
    | Some map ->
      let acc1 =
        Public_data.YearExtendedMap.collect
          year
          map
          []
      in
      let acc2 =
        fold_left_rev
          (Public_data.ProgramExtendedMap.collect
             program)
            acc1
            []
        in
        fold_left_rev
          (Public_data.AcronymExtendedMap.collect  dpt)
            acc2  []


let add_decision
    warn unify (pos:string * int * int *int) state
    (decision:Public_data.decision) (decisions:t)  =
  let program = decision.Public_data.decision_program in
  let year = decision.Public_data.decision_annee in
  let firstname = decision.Public_data.decision_firstname in
  let lastname = decision.Public_data.decision_lastname in
  let dpt = decision.Public_data.decision_dpt in
  let decision_opt' =
    get_decision ~program ~dpt ~year ~firstname ~lastname decisions
  in
  let state, decision =
    match decision_opt' with
    | [] -> state, decision
    | [decision'] -> unify pos state decision decision'
    | _::_::_ ->
      warn
        pos
        "Several decisions for the same diploma, student and year"
        Exit
        state,
      decision
  in
  let map1 =
    match
      Public_data.LastNameMap.find_opt lastname decisions
    with
    | None -> Public_data.FirstNameMap.empty
    | Some map1 -> map1
  in
  let map2 =
    match
      Public_data.FirstNameMap.find_opt firstname map1
    with
    | None -> Public_data.YearMap.empty
    | Some map2 -> map2
  in
  let map3 =
    match
      Public_data.YearMap.find_opt year map2
    with
    | None -> Public_data.ProgramMap.empty
    | Some map3 -> map3
  in
  let map4 =
    match
      Public_data.ProgramMap.find_opt program map3
    with
    | None -> Public_data.AcronymMap.empty
    | Some map4 -> map4
  in
  state,
  Public_data.LastNameMap.add
    lastname
    (Public_data.FirstNameMap.add
       firstname
       (Public_data.YearMap.add
          year
          (Public_data.ProgramMap.add
             program
             (Public_data.AcronymMap.add
                dpt
                decision
                map4)
             map3)
          map2)
       map1)
    decisions
