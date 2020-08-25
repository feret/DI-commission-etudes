type t =
  Public_data.admission
    Public_data.YearMap.t
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

let empty = Public_data.LastNameMap.empty

let get_admission ~firstname ~lastname ~year admissions =
  match
    Public_data.LastNameMap.find_opt
      lastname
      admissions
  with
  | None -> None
  | Some map
    ->
    match
      Public_data.FirstNameMap.find_opt
        firstname
        map
    with
    | None -> None
    | Some map ->
      Public_data.YearMap.find_opt
        year
        map

let add_admission
    unify pos state
    admission admissions  =
  let year = admission.Public_data.admission_annee in
  let firstname = admission.Public_data.admission_firstname in
  let lastname = admission.Public_data.admission_lastname in
  let admission_opt' =
    get_admission ~year ~firstname ~lastname admissions
  in
  let state, admission =
    match admission_opt' with
  | None -> state, admission
  | Some admission' -> unify pos state admission admission'
  in
  let map1 =
    match
      Public_data.LastNameMap.find_opt lastname admissions
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
  state,
  Public_data.LastNameMap.add
    lastname
    (Public_data.FirstNameMap.add
       firstname
       (Public_data.YearMap.add
          year
          admission
          map2)
       map1)
    admissions
