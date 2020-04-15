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
  let () = Format.printf "Extracting %s @." output in
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

let get_last_line file =
  let source = open_in file in
  let rec aux last_opt =
    try
      aux (Some (input_line source))
    with
      End_of_file -> last_opt
  in
  let last_opt = aux None in
  let () = close_in source in
  last_opt

let checkoutput output =
  let chan = open_in output in
  let () =
    try
      let _ = input_line chan in
      Format.printf "COMPLETE @."
    with End_of_file ->
      Format.printf "GPS EXTRACTION FAILED"
  in
  close_in chan

let check ?log_file ?log_repository ~output_repository ~output_file_name ~period ?timeout file_retriever state =
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
    let output =
      if output_repository = ""
      then output_file_name
      else
        Printf.sprintf "%s/%s"
          output_repository output_file_name
    in
    let rec aux total_time =
      if
        match timeout with
        | None -> false
        | Some timeout -> total_time > timeout
      then
        let () = Format.printf "TIME OUT @." in
        state
      else
        let () = Unix.sleep period in
        match file_retriever with
        | Public_data.WGET ->
          begin
            match
              get_last_line log
            with
            | None -> aux (total_time + period)
            | Some line ->
              let size = String.length line in
              if size < 3 then
                let () = checkoutput output in
                state
              else
                let three_last =
                  String.sub line (size-4) 3
                in
                if three_last = "..."
                then
                  let () = Format.printf "." in
                  let () = Format.print_flush () in
                  aux (total_time + period)
                else
                  let () = checkoutput output in
                  state
          end
    in
     aux 0
