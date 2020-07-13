
type t =
  Public_data.cursus_exception
    Public_data.YearMap.t
    Public_data.FirstNameMap.t
    Public_data.LastNameMap.t
    Public_data.CodeMap.t

let empty = Public_data.CodeMap.empty

let get_cursus_exception
    ~firstname ~lastname ~code_gps ~year
    cursus_exceptions =
  match
    Public_data.CodeMap.find_opt
      code_gps
      cursus_exceptions
  with
  | None -> None
  | Some map ->
    begin
      match Public_data.LastNameMap.find_opt
              lastname map
      with
      | None -> None
      | Some map ->
        begin
          match Public_data.FirstNameMap.find_opt firstname map
          with
          | None -> None
          | Some map ->
            begin
            match Public_data.YearMap.find_opt year map
            with
            | None -> None
            | Some cursus_exception -> Some cursus_exception
            end
        end
    end


let add_cursus_exception
    unify pos state
    cursus_exception cursus_exceptions =
  let code_gps = cursus_exception.Public_data.codecours in
  let firstname = cursus_exception.Public_data.student_firstname in
  let lastname = cursus_exception.Public_data.student_lastname in
  let year = cursus_exception.Public_data.annee_de_validation in
  let cursus_exception' =
    get_cursus_exception
      ~code_gps ~firstname ~lastname ~year cursus_exceptions
  in
  let state, cursus_exception =
    match cursus_exception' with
    | None -> state, cursus_exception
    | Some b ->
      unify pos state cursus_exception b
  in
  let cursus_exceptions =
    let mapa =
      match
        Public_data.CodeMap.find_opt
          code_gps
          cursus_exceptions
      with
      | None -> Public_data.LastNameMap.empty
      | Some map -> map
    in
    let mapb =
      match Public_data.LastNameMap.find_opt
              lastname mapa
      with
      | None -> Public_data.FirstNameMap.empty
      | Some map -> map
    in
    let mapc =
      match Public_data.FirstNameMap.find_opt
              firstname mapb
      with
      | None -> Public_data.YearMap.empty
      | Some map -> map
    in
    Public_data.CodeMap.add
      code_gps
      (Public_data.LastNameMap.add
         lastname
         (Public_data.FirstNameMap.add
            firstname
            (Public_data.YearMap.add
               year cursus_exception mapc)
            mapb
         )
         mapa
      )
      cursus_exceptions
  in
  state, cursus_exceptions

let dump t =
  let () =
    Public_data.CodeMap.iter
    (fun c map ->
       Public_data.LastNameMap.iter
         (fun n map ->
            Public_data.FirstNameMap.iter
              (fun p map ->
                 Public_data.YearMap.iter
                   (fun y cursus_exception ->
                      Format.printf
                        "%s %s %s %s %s %s %s %s %s %s %s %s %s @. "
                        c (Special_char.correct_string c)
                        n (Special_char.correct_string n)
                        p (Special_char.correct_string p)
                        y (Special_char.correct_string y)
                        cursus_exception.Public_data.codecours
                        cursus_exception.Public_data.student_firstname  cursus_exception.Public_data.student_lastname cursus_exception.Public_data.annee_de_validation cursus_exception.Public_data.class_dpt
                   )
                   map ) map ) map) t
  in
  let () = Format.print_newline () in
  let () = Format.print_flush () in ()
