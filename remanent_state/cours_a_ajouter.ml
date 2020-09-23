type t =
  Public_data.cours_a_ajouter list
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t

let empty = Public_data.LastNameMap.empty

let get_additional_courses ~firstname ~lastname
    cours_a_ajouter =
  match
    Public_data.LastNameMap.find_opt
      lastname
      cours_a_ajouter
  with
  | None -> []
  | Some a ->
    begin
      match
        Public_data.FirstNameMap.find_opt
          firstname
          a
      with
      | None -> []
      | Some a -> a
    end

let add_additional_course
  _pos state
    course_elt course_map =
  let lastname = course_elt.Public_data.coursaj_nom in
  let firstname = course_elt.Public_data.coursaj_prenom in
  let mapa =
    match
      Public_data.LastNameMap.find_opt
        lastname
        course_map
    with
    | None -> Public_data.FirstNameMap.empty
    | Some a -> a
  in
  let old =
    match
      Public_data.FirstNameMap.find_opt
        firstname
        mapa
    with
    | None -> []
    | Some a -> a
in
state,
Public_data.LastNameMap.add
  lastname
  (Public_data.FirstNameMap.add
     firstname
     (course_elt::old)
     mapa)
  course_map
