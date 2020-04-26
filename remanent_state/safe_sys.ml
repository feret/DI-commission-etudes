let file_exists _pos state x =
  state, Sys.file_exists x

let is_directory _pos state x =
  state, Sys.is_directory x

let command pos state s =
  try
    if Sys.command s = 0
    then state
    else
      Remanent_state.warn
        pos (Printf.sprintf "\"Sys.command %s\" failed" s)
        Exit state
  with
  | exn ->
    Remanent_state.warn
      pos (Printf.sprintf "\"Sys.command %s\" failed" s)
      exn state

let rec_mk_when_necessary pos state output_repository =
  if Sys.is_directory output_repository
  then state, output_repository
  else
    let state =
      command
        pos
        state
        (Printf.sprintf "mkdir %s" output_repository)
    in
    state, output_repository

let readdir _pos state x =
  state, Sys.readdir x

let getcwd _pos state = state, Sys.getcwd ()
let chdir _pos state x =
  let () = Sys.chdir x in state 
