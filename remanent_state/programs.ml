type t = Public_data.program Public_data.CodeMap.t

let empty = Public_data.CodeMap.empty


let unify ~safe_mode logger prefix pos error program program'
  =
  if
    Special_char.correct_string program.Public_data.code_gps
    =
    Special_char.correct_string program'.Public_data.code_gps
    &&
    Tools.map_opt
      Special_char.correct_string program.Public_data.dpt_acronym
    =
    Tools.map_opt
      Special_char.correct_string program'.Public_data.dpt_acronym
    &&
    Tools.map_opt
      Special_char.correct_string program.Public_data.level
    =
    Tools.map_opt
      Special_char.correct_string program'.Public_data.level
    &&
    Tools.map_opt
      Special_char.correct_string program.Public_data.label
    =
    Tools.map_opt
      Special_char.correct_string program'.Public_data.label
  then
      error, program
  else
  let message =
    Format.sprintf
      "Cannot unify program data with  %s %s %s %s VS %s %s %s %s"
      program.Public_data.code_gps
      (match program.Public_data.dpt_acronym with
         None -> "" | Some x -> x)
      (match program.Public_data.level with
         None -> "" | Some x -> x)
      (match program.Public_data.label with
         None -> "" | Some x -> x)
      program'.Public_data.code_gps
      (match program'.Public_data.dpt_acronym with
         None -> "" | Some x -> x)
      (match program'.Public_data.level with
         None -> "" | Some x -> x)
      (match program'.Public_data.label with
         None -> "" | Some x -> x)
  in
  Exception.warn
    logger
    ~safe_mode
    ~message
    prefix
    error
    pos
    Exit
    program

let get_program  ~code_gps program =
  let code_gps =
    String.lowercase_ascii code_gps
  in
    Public_data.CodeMap.find_opt
      code_gps
      program


let add_program
    ~safe_mode logger prefix pos error
    program programs =
  let code_gps = program.Public_data.code_gps in
  let program' = get_program ~code_gps programs in
  let error, program =
    match program' with
    | None -> error, program
    | Some b ->
      unify ~safe_mode logger prefix pos error
        program b
  in
  let programs =
    Public_data.CodeMap.add
      code_gps
      program
      programs
  in
  error, programs
