let launch
    ?user_name ?password ?tool ?options
    t ~url ~output_repository ~output_file_name
  =
  let output =
    if output_repository = ""
    then output_file_name
    else
      Printf.sprintf "%s/%s"
        output_repository output_file_name
  in
  let tool =
    match tool with
    | None ->
      begin
        match t with
        | Public_data.WGET ->
          Printf.sprintf "wget -o %s" output
      end
    | Some tool -> tool
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
