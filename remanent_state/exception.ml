type method_handler = Exception_without_parameter.method_handler
let empty_error_handler = Exception_without_parameter.empty_error_handler
let is_empty_error_handler = Exception_without_parameter.is_empty_error_handler

let safe_warn prefix logger file_name message exn =
  let uncaught = Exception_without_parameter.build_uncaught_exception ?file_name ?message exn in
  let stringlist =
    Exception_without_parameter.stringlist_of_uncaught
      uncaught [prefix]
  in
  let _ =
    List.iter
      (Loggers.fprintf logger "%s")
      stringlist
  in
  let _ = Loggers.print_newline logger in
  raise (Exception_without_parameter.Uncaught_exception uncaught)

let unsafe_warn
    error_handler file_name message exn default
  =
  let uncaught =
    Exception_without_parameter.build_uncaught_exception
      ?file_name ?message exn
  in
  Exception_without_parameter.add_uncaught_error
    uncaught error_handler, default ()

let warn_aux
    logger prefix ~safe_mode error_handler
    file_name message exn default =
  let error,dft =
    if safe_mode
    then
      unsafe_warn
        error_handler
        file_name message exn default
    else
      safe_warn
        prefix logger file_name message exn
  in
  error,dft

let warn_with_exn
    logger prefix ~safe_mode  error_handler (file,line,_,_) ?message:(message="")
    exn default =
  let liaison =
    if message = ""  then "" else ": "
  in
  warn_aux
    logger prefix ~safe_mode
    error_handler
    (Some file)
    (Some ("line "^(string_of_int line)^liaison^message))
    exn default

let warn
    logger prefix ~safe_mode error_handler
    file_line ?message:(message="") exn default =
  warn_with_exn logger prefix ~safe_mode  error_handler file_line ~message exn (fun () -> default)

let print_core logger prefix error_handler =
  let prefix = prefix^"error:" in
  let _ =
    List.iter
      (fun caught ->
         let stringlist = prefix::(Exception_without_parameter.stringlist_of_caught caught []) in
         let _ =
           List.iter
             (Loggers.fprintf logger "%s") stringlist
         in
         let _ = Loggers.print_newline logger in
         ())
      (List.rev (Exception_without_parameter.get_caught_exception_list error_handler))
  in
  let _ =
    List.iter
      (fun uncaught ->
         let stringlist =  prefix::(Exception_without_parameter.stringlist_of_uncaught uncaught []) in
         let _ = List.iter (Loggers.fprintf logger "%s") stringlist in
         let _ = Loggers.print_newline logger in
         ())
      (List.rev (Exception_without_parameter.get_uncaught_exception_list error_handler))
  in
  ()

let print logger prefix error_handler =
  if
    Exception_without_parameter.get_caught_exception_list error_handler = []
    &&
    Exception_without_parameter.get_uncaught_exception_list error_handler = []
  then
    let () = Loggers.fprintf logger "%sexecution finished without any exception" prefix in
    let () = Loggers.print_newline logger in
    ()
  else
    let () = Loggers.fprintf logger "%sSome exceptions have been raised" prefix in
    let () = Loggers.print_newline logger in
    print_core logger prefix error_handler

let check_point
    warn parameter error error'
    s ?message exn =
  if error==error'
  then error
  else
    let error,() = warn parameter error' s ?message exn () in
    error
