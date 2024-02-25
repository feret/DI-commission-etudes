type t =
      Public_data.course_pegasus
        Public_data.CodeMap.t
          Public_data.YearMap.t

let empty =
      Public_data.YearMap.empty

let get_pegasus_course ~code ~year  courses =
  let code =
    String.lowercase_ascii code
  in
  match
    Public_data.YearMap.find_opt
      year
      courses
  with
  | None -> None
  | Some a ->
      Public_data.CodeMap.find_opt
        code
        a

let add_pegasus_course
    unify pos state
    course courses =
  let code = course.Public_data.pegasus_helisa in
  let year = course.Public_data.pegasus_year in
  let course' = get_pegasus_course ~code ~year  courses  in
  let state, course =
    match course' with
    | None -> state, course
    | Some course' ->
        unify pos state course course'
  in
  let courses =
    let old_year =
      match
        Public_data.YearMap.find_opt
          code
          courses
      with
      | Some map -> map
      | None -> Public_data.CodeMap.empty
    in
    Public_data.YearMap.add year
      (Public_data.CodeMap.add code course old_year) courses
  in
  state, courses
