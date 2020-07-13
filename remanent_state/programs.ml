type t = Public_data.program Public_data.CodeMap.t

let empty = Public_data.CodeMap.empty

let get_program  ~code_gps program =
  let code_gps =
    String.lowercase_ascii code_gps
  in
    Public_data.CodeMap.find_opt
      code_gps
      program


let add_program
    unify pos state
    program programs =
  let code_gps = program.Public_data.code_gps in
  let program' = get_program ~code_gps programs in
  let state, program =
    match program' with
    | None -> state, program
    | Some b ->
      unify pos state program b
  in
  let programs =
    Public_data.CodeMap.add
      code_gps
      program
      programs
  in
  state, programs
