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
  let list_of_reps =
    String.split_on_char '/' output_repository
  in
  let rec aux state list output =
    match list with
    | [] ->
      state, output
    | h::t ->
      let new_repository =
        output^"/"^h
      in
      let rec aux2 state new_repository =
        let state, bool =
          try
            state, Sys.is_directory new_repository
          with
          | Sys_error x ->
            try
              if Sys.command (Printf.sprintf "mkdir %s" new_repository) = 0
              then state, true
              else
                let _ =
                  Remanent_state.stop pos
                    (Printf.sprintf "Cannot create repository %s (%s)" new_repository x)
                    Exit state
                in state, true
            with
              Sys_error x ->
              let state =
              Remanent_state.stop pos
                (Printf.sprintf "Cannot create repository %s (%s)" new_repository x)
                Exit state
              in
              state, true
        in
        if bool then state, new_repository
        else
          aux2 state ("new_repository"^"~")
      in
      let state, new_repository = aux2 state new_repository in
      aux state t new_repository
  in
  let state, repository = aux state list_of_reps "" in
  state, repository

let readdir _pos state x =
  state, Sys.readdir x

let getcwd _pos state = state, Sys.getcwd ()
let chdir _pos state x =
  let () = Sys.chdir x in state

let rm pos state filename =
  try
    let () = Sys.remove filename in state
  with
  | Sys_error s ->
    Remanent_state.warn
      pos
      s
      (Sys_error s)
      state
