type dump =
  ?output_repository:string ->
  ?prefix:string ->
  ?file_name:string ->
  Remanent_state.t ->
  Remanent_state.t

let dump_elts
    ?output_repository ?prefix ?file_name
    ?event_opt
    ~get ~get_repository ~default_file_name
    ~cmp ~headers ~columns state  =
  let state =
    Remanent_state.open_event_opt event_opt state
  in
  let state, elts =
    get state
  in
  match elts with
  | [] -> state
  | _ ->
    let state, prefix =
      match prefix with
      | None -> state, ""
      | Some prefix -> state, prefix
    in
    let state, output_repository =
      match output_repository with
      | None ->
        get_repository state
      | Some rep -> state, rep
    in
    let state, output_file_name =
      match file_name with
      | None -> state, default_file_name
      | Some file_name -> state, file_name
    in
    let output_repository =
      match output_repository,prefix  with
      | ".",prefix | "",prefix -> prefix
      | x,"" -> x
      | x1,x2 ->
        Printf.sprintf "%s/%s" x1 x2
    in
    let state, output_repository =
      Safe_sys.rec_mk_when_necessary
        __POS__
        state output_repository
    in
    let file =
      if output_repository = ""
      then output_file_name
      else
        Printf.sprintf "%s/%s"
          output_repository output_file_name
    in
    let state, output_channel_opt =
      try
        state, Some (open_out file)
      with _ ->
        let () =
          Format.printf
            "Cannot open file %s@."
            file
        in
        Remanent_state.warn
          __POS__
          (Format.sprintf "Cannot open file %s"  file)
          Exit
          state ,
        None
    in
    let state =
      match output_channel_opt with
      | None -> state
      | Some out ->
        let mode = Loggers.HTML in
        let logger = Loggers.open_logger_from_channel ~mode
            out in
        let extended_elts =
          Tools.prepare_report
            ~cmp
            ~headers:(List.rev_map (fun (_,_,a) -> a)
                        (List.rev headers))
            elts
        in
        let () =
          Tools.dump_report
            ~print_header:(Loggers.print_headers logger)
            ~open_row:(fun () -> Loggers.open_row logger)
            ~close_row:(fun () -> Loggers.close_row logger)
            ~print_cell:(Loggers.print_cell logger)
            ~close_array:(fun () -> Loggers.close_array logger)
            ~string_of_headers:(List.rev_map (fun (a,b,_) ->
                a,b) (List.rev headers))
            ~string_of_column:columns
            ~open_array:(Loggers.open_array logger)
            extended_elts in
        let () = Loggers.flush_logger logger in
        state
    in
    let () =
      match output_channel_opt with
      | Some chan -> close_out chan
      | None -> ()
    in
    let state =
      Remanent_state.close_event_opt event_opt state
    in
    state

let lift_cmp f a b =
  compare (f a) (f b)
let op_cmp cmp a b = cmp b a
