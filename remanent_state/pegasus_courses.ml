type t =
    {per_code:  Public_data.course_pegasus
        Public_data.CodeMap.t
          Public_data.YearMap.t ;
     per_libelle: Public_data.course_pegasus list
        Public_data.StringOptMap.t
         Public_data.StringMap.t
           Public_data.YearMap.t }

let empty =
      {per_code = Public_data.YearMap.empty ; per_libelle = Public_data.YearMap.empty}

let get_pegasus_course ~code ~year  courses =
  let code =
    Special_char.lowercase code
  in
  match
    Public_data.YearMap.find_opt
      year
      courses.per_code
  with
  | None -> None
  | Some a ->
      Public_data.CodeMap.find_opt
        code
        a

let get_pegasus_course_by_libelle ~libelle ~year  ~semester courses =
    let libelle =
        Special_char.lowercase libelle
    in
    let semester =
        Tools.map_opt Special_char.lowercase semester
    in
    match
        Public_data.YearMap.find_opt
            year
            courses.per_libelle
    with
          | None -> []
          | Some a ->
              match
                Public_data.StringMap.find_opt
                  libelle
                  a
              with
                | None -> []
                | Some a ->
                  match Public_data.StringOptMap.find_opt
                      semester a
                  with None -> [] | Some a -> a

let get_pegasus_course_by_libelle ?domain ~libelle ~year  ~semester courses =
    let l = get_pegasus_course_by_libelle ~libelle ~year  ~semester courses in
    let l = match domain with None -> l | Some _ -> List.filter (fun a -> a.Public_data.pegasus_domain = domain) l in
    List.sort
        (fun a b -> compare a.Public_data.pegasus_domain b.Public_data.pegasus_domain)
        l

let add_pegasus_course
    unify pos state
    course courses =
  let code = course.Public_data.pegasus_helisa in
  let libelle = course.Public_data.pegasus_libelle in
  let year = course.Public_data.pegasus_year in
  let semester = course.Public_data.pegasus_semester in
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
          year
          courses.per_code
      with
      | Some map -> map
      | None -> Public_data.CodeMap.empty
    in
    let old_year' =
      match
        Public_data.YearMap.find_opt
          year
          courses.per_libelle
      with
      | Some map -> map
      | None -> Public_data.StringMap.empty
    in
    let libelle' =
      match
        Public_data.StringMap.find_opt
          libelle
          old_year'
      with
        | Some map -> map
        | None -> Public_data.StringOptMap.empty
    in
    let course_list =
      match
        Public_data.StringOptMap.find_opt
          semester
          libelle'
      with
        | Some c -> c
        | None -> []
    in
    {per_code = Public_data.YearMap.add year
      (Public_data.CodeMap.add code course old_year) courses.per_code ;
     per_libelle =
        Public_data.YearMap.add year
            (Public_data.StringMap.add libelle
                (Public_data.StringOptMap.add semester (course::course_list) libelle')
                old_year')
            courses.per_libelle }
  in
  state, courses



let dump m =
    let () = Format.printf "DUMP COURSES @." in
    Public_data.YearMap.iter
          (fun y ->
              Public_data.StringMap.iter
                (fun lib ->
                    Public_data.StringOptMap.iter
                      (fun sem_opt courses ->
                        List.iter (fun course -> Format.printf "%s %s (%s) %s @." y lib (Tools.unsome_string sem_opt) course.Public_data.pegasus_helisa) courses)))
 m.per_libelle
