type t =
  {
    per_year:
      Public_data.course_name_translation Public_data.CodeMap.t Public_data.YearMap.t ;
    per_code:
      Public_data.course_name_translation Public_data.YearMap.t
        Public_data.CodeMap.t ;
  }

let empty =
  {
    per_year =
      Public_data.YearMap.empty;
    per_code =
      Public_data.CodeMap.empty;
  }

let get_course_name_translation ~strong ~year ~codegps course_name_translations =
  let codegps =
    Special_char.lowercase codegps
  in
  match
    Public_data.CodeMap.find_opt
      codegps
      course_name_translations.per_code
  with
  | None -> None
  | Some yearmap ->
    if strong
    then
      Public_data.YearMap.find_opt
        year yearmap
    else
      Public_data.YearMap.fold
        (fun year' course_name_translation output ->
           if compare year' year <= 0
           then Some course_name_translation
           else output)
        yearmap None


let add_course_name_translation
    unify pos state
    course_name_translation course_name_translations
  =
  let codegps = course_name_translation.Public_data.code in
  let year = course_name_translation.Public_data.year in
  let course_name_trans_opt' =
    get_course_name_translation
      ~strong:true ~codegps ~year course_name_translations
  in
  let state, course_name_translation =
    match course_name_trans_opt' with
    | None -> state, course_name_translation
    | Some course_name_translation' ->
        unify pos state course_name_translation course_name_translation'
  in
  let course_name_translations =
    let per_year =
      let old_year =
        match
          Public_data.YearMap.find_opt
            year
            course_name_translations.per_year
        with
        | Some map -> map
        | None -> Public_data.CodeMap.empty
      in
      Public_data.YearMap.add
        year
        (Public_data.CodeMap.add
           codegps
           course_name_translation
           old_year
        )
        course_name_translations.per_year
    in
    {course_name_translations with per_year}
  in
  let course_name_translations =
    let per_code =
      let old_code =
        match
          Public_data.CodeMap.find_opt
            codegps
            course_name_translations.per_code
        with
        | Some map -> map
        | None -> Public_data.YearMap.empty
      in
      Public_data.CodeMap.add
        codegps
        (Public_data.YearMap.add
           year
           course_name_translation
           old_code)
        course_name_translations.per_code
    in
    {course_name_translations with per_code}
  in
  state, course_name_translations

let get_course_name_translation = get_course_name_translation ~strong:false

type tentry =
  Public_data.course_entry Public_data.StringMap.t

let empty_course_entry = Public_data.StringMap.empty

let get_course_entry gps_entry course_entries=
  let gps_entry =
    Special_char.lowercase gps_entry
  in
  Public_data.StringMap.find_opt
    gps_entry
    course_entries

let add_course_entry
    unify pos state
    course_entry course_entries
  =
  let label = course_entry.Public_data.gps_entry in
  let () = Format.printf "%s" label in 
  let course_entry_opt' =
    get_course_entry label course_entries
  in
  let state, course_entry =
    match course_entry_opt' with
    | None -> state, course_entry
    | Some course_entry' ->
      unify pos state course_entry course_entry'
  in
  state,
  Public_data.StringMap.add label course_entry course_entries

let to_list a =
  List.rev
    (Public_data.StringMap.fold
       (fun _ elt list -> elt::list)
       a [])
