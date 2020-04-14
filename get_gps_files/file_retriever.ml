let launch
    ?user_name ?password ?tool ?options
    t url file_name
=
  let tool =
    match tool with
    | None ->
      begin
        match t with
        | Public_data.WGET -> "wget"
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
      "%s %s %s > %s"
      tool options url
      file_name
  in
  Sys.command command
