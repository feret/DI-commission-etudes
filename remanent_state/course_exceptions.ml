type t =
  {
    per_year:
      Public_data.course_exception Public_data.CodeMap.t Public_data.YearMap.t ;
    per_code:
      Public_data.course_exception Public_data.YearMap.t
        Public_data.CodeMap.t ;
  }

let empty =
  {
    per_year =
      Public_data.YearMap.empty;
    per_code =
      Public_data.CodeMap.empty;
  }

let get_course_exception ~strong ~year ~codegps course_exceptions =
  let codegps =
    Special_char.lowercase codegps
  in
  match
    Public_data.CodeMap.find_opt
      codegps
      course_exceptions.per_code
  with
  | None -> None
  | Some yearmap ->
    if strong
    then
      Public_data.YearMap.find_opt
        year yearmap
    else
      Public_data.YearMap.fold
        (fun year' course_exception output ->
           if compare year' year <= 0
           then Some course_exception
           else output)
        yearmap None


let add_course_exception
    unify pos state
    course_exception course_exceptions
  =
  let codegps = course_exception.Public_data.course_exception_code in
  let year = course_exception.Public_data.course_exception_year in
  let course_exn_opt' =
    get_course_exception  ~strong:true ~codegps ~year course_exceptions
  in
  let state, course_exception =
    match course_exn_opt' with
    | None -> state, course_exception
    | Some course_exception' ->
        unify pos state course_exception course_exception'
  in
  let course_exceptions =
    let per_year =
      let old_year =
        match
          Public_data.YearMap.find_opt
            year
            course_exceptions.per_year
        with
        | Some map -> map
        | None -> Public_data.CodeMap.empty
      in
      Public_data.YearMap.add
        year
        (Public_data.CodeMap.add
           codegps
           course_exception
           old_year
        )
        course_exceptions.per_year
    in
    {course_exceptions with per_year}
  in
  let course_exceptions =
    let per_code =
      let old_code =
        match
          Public_data.CodeMap.find_opt
            codegps
            course_exceptions.per_code
        with
        | Some map -> map
        | None -> Public_data.YearMap.empty
      in
      Public_data.CodeMap.add
        codegps
        (Public_data.YearMap.add
           year
           course_exception
           old_code)
        course_exceptions.per_code
    in
    {course_exceptions with per_code}
  in
  state, course_exceptions

let get_course_exception = get_course_exception ~strong:false
