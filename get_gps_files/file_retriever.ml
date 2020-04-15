let launch
    ?user_name ?password ?options
    ?log_file ?log_repository ?timeout
    t ~url ~output_repository ~output_file_name
  =
  let output =
    if output_repository = ""
    then output_file_name
    else
      Printf.sprintf "%s/%s"
        output_repository output_file_name
  in
  let log =
    match log_repository,log_file with
    | _,None -> None
    | None, Some x -> Some x
    | Some x,Some y ->
      Some (Printf.sprintf "%s/%s" x y)
  in
  let log_option =
    match log with
    | None -> ""
    | Some x -> Printf.sprintf " -o %s" x
  in
  let timeout =
    match timeout with
    | None -> ""
    | Some i -> Printf.sprintf " -T %d" i
  in
  let tool =
    match t with
    | Public_data.WGET ->
      Printf.sprintf "wget -O %s%s%s" output log_option timeout
  in
  let options =
    match options with
    | None -> ""
    | Some opt -> opt
  in
  let options =
    Printf.sprintf "%s%s%s"
      options
      (match user_name with
       | None -> ""
       | Some x -> Printf.sprintf " -u %s" x)
      (match password with
       | None -> ""
       | Some x -> Printf.sprintf " -p %s" x)
  in
  let command =
    Printf.sprintf
      "%s %s %s "
      tool options url
  in
  Sys.command command

let check ?log_file ?log_repository ~period ?timeout file_retriever state =
  let log =
    match log_repository,log_file with
    | _,None -> None
    | None, Some x -> Some x
    | Some x,Some y ->
      Some (Printf.sprintf "%s/%s" x y)
  in
  match log with
  | None -> state
| Some log ->
    let rec aux total_time =
      if
        match timeout with
        | None -> false
        | Some timeout -> total_time > timeout
      then
        let () = Printf.printf "TIME OUT @." in
        state
      else
        let () = Unix.sleep period in
        match file_retriever with
        | Public_data.WGET ->
          begin
            let source = open_in log in
            let rec get_last_line last_opt =
              try
                get_last_line (Some (input_line source))
              with
                End_of_file -> last_opt
            in
            let _ = close_in source in
            match
              get_last_line None
            with
            | None -> aux (total_time + period)
            | Some line ->
              let size = String.length line in
              if size < 3 then
                let () = Printf.printf "COMPLETE @." in
                state
              else
                let three_last =
                  String.sub line (size-4) 3
                in
                if three_last = "..."
                then
                  let () = Printf.printf "WAITING @." in
                  aux (total_time + period)
                else
                  let () = Printf.printf "COMPLETE @." in
                  state
          end
    in
  aux 0
