type step_kind =
  | Dummy
  | Initialisation
  | Cloud_synchronization
  | Extract_gps_data_base
  | Extract_gps_file of string * string
  | Extract_gps_file_from_handmade_files of string * string
  | Extract_gps_file_from_backup_files of string * string
  | Extract_gps_file_from_database of string * string * string option
  | Collect_picture of string * string * string
  | Collect_picture_from_url of string * string * string * string
  | Collect_record_from_url of string * string * string * string
  | Patch_gps_file of string option
  | Build_keywords_automaton
  | Export_transcript of string option
  | Collect_additional_courses
  | Collect_admissions
  | Collect_scholarships
  | Collect_mentoring
  | Collect_minors
  | Collect_majors
  | Collect_departement
  | Collect_dens_candidates
  | Collect_program
  | Collect_cursus_exceptions
  | Collect_decisions
  | Collect_dispenses
  | Collect_compensations
  | Collect_inscriptions
  | Collect_cursus
  | Collect_course_exceptions
  | Collect_modified_grade
  | Collect_course_name_translations
  | Collect_course_entries
  | Collect_sorted_courses
  | Dump_course_entries
  | Dump_courses_to_be_sorted
  | Dump_missing_course_translation
  | Dump_missing_grades
  | Dump_missing_ects_attributions
  | Dump_missing_mentors
  | Dump_missing_internship_descriptions
  | Dump_ambiguous_internship_descriptions
  | Dump_mentor_list
  | Dump_national_diploma_list
  | Dump_dens_result
  | Dump_student_list
  | Dump_mineures_suggestions
  | Dump_dens_candidate_suggestions
  | Scan_csv_files of string * string



type step =
  {
    tag: step_kind;
    time_start: float;
    duration: float option;
  }

let string_of_step_kind x =
  match
    x
  with
  | Dummy -> "Dummy"
  | Initialisation -> "Initialization"
  | Cloud_synchronization -> "Cloud synchronization"
  | Extract_gps_data_base -> "Extract GPS data-base"
  | Extract_gps_file (lastname,firstname)->
    Printf.sprintf
      "Extract GPS file (%s %s)"
      lastname firstname
  | Extract_gps_file_from_handmade_files (lastname,firstname)->
    Printf.sprintf
      "Check whether there is a handmade file (%s %s)"
      lastname firstname
  | Extract_gps_file_from_backup_files (lastname,firstname)->
    Printf.sprintf
      "Look for a backup file (%s %s)"
      lastname firstname
  | Extract_gps_file_from_database (lastname,firstname,dpt_opt)->
    Printf.sprintf
      "Extract GPS file (%s %s %s) from ENS database"
      lastname firstname
      (match dpt_opt with None -> "" | Some x -> x)
  | Collect_picture (lastname, firstname, promo) ->
    Printf.sprintf
      "Extract photo (%s %s %s) from ens member directory"
      lastname firstname promo
  | Collect_record_from_url (lastname, firstname, promo, url) ->
    Printf.sprintf
      "Try url %s to extract the record of (%s %s %s) from ens member directory"
      url lastname firstname promo
  | Collect_picture_from_url (lastname, firstname, promo, url) ->
    Printf.sprintf
      "Try url %s to extract the picture of (%s %s %s) from ens member directory"
      url lastname firstname promo
  | Collect_course_name_translations ->
      "Collect course name translations"
  | Patch_gps_file None ->
    "Patch GPS output"
  | Patch_gps_file (Some file) ->
    Printf.sprintf "Patch GPS output (%s)" file
  | Build_keywords_automaton -> "Build keywords automaton"
  | Export_transcript None ->
    Printf.sprintf
      "Export transcript"
  | Export_transcript (Some x) ->
    Printf.sprintf
      "Export transcript (%s)"
      x
  | Collect_additional_courses -> "Collect data about not-in-gps courses"
  | Collect_dens_candidates -> "Collect dens candidates"
  | Collect_scholarships -> "Collect data about scholarships"
  | Collect_mentoring -> "Collect data about mentoring"
  | Collect_minors -> "Collect data about minors"
  | Collect_majors -> "Collect data about majors"
  | Collect_departement -> "Collect data about teaching departments"
  | Collect_program -> "Collect data about teaching programs"
  | Collect_inscriptions -> "Collect inscriptions"
  | Collect_cursus_exceptions -> "Collect data about cursus exceptions"
  | Collect_decisions -> "Collect data about programs jury"
  | Collect_dispenses -> "Collect data about diploma dispenses"
  | Collect_compensations -> "Collect data about grades compensations"
  | Collect_admissions -> "Collect data about admission to external M2"
  | Collect_cursus -> "Collect notices about cursus"
  | Collect_course_exceptions -> "Collect course exceptions"
  | Collect_modified_grade -> "Collect modified grades"
  | Collect_course_entries -> "Collect course entries"
  | Collect_sorted_courses -> "Collect sorted courses"
  | Dump_missing_grades -> "Dump missing notes"
  | Dump_missing_ects_attributions-> "Dump missing ects attributions"
  | Dump_missing_mentors -> "Dump missing mentors"
  | Dump_missing_internship_descriptions -> "Dump missing internship description"
  | Dump_ambiguous_internship_descriptions -> "Dump ambiguous internship description"
  | Dump_mentor_list -> "Dump list of mentors"
  | Dump_national_diploma_list -> "Dump national diploma result"
  | Dump_dens_result -> "Dump DENS result"
  | Dump_student_list -> "Dump student list"
  | Dump_course_entries -> "Dump course entries"
  | Dump_courses_to_be_sorted -> "Dump courses to be sorted"
  | Dump_missing_course_translation -> "Dump missing course translation"
  | Dump_mineures_suggestions -> "Dump Mineures/Majeures suggestion"
  | Dump_dens_candidate_suggestions -> "Dump dens candidate suggestion"
  | Scan_csv_files (rep,file) -> (Format.sprintf "Scan %s/%s" rep file)

let print_step_kind logger x =
  Loggers.print_cell logger
    (string_of_step_kind x)

let k_first k l =
  let rec aux k l output =
    if k=0 then [],List.rev output
    else
      match
        l
      with
      | [] ->
        (let rec aux k output =
           if k = 0 then output else aux (k-1)
               (""::output) in aux k []),List.rev output
      | t::q -> aux (k-1) q (t::output)
  in aux k l []

let print_task logger (a,b) =
  let _ = print_step_kind logger a.tag in
  let tab,b = k_first 4 b in
  let _ =
    List.iter
      (print_step_kind logger)
      b
  in
  let () =
    List.iter
      (Loggers.print_cell logger) tab
  in
  let _ =
    Loggers.print_cell logger
      begin
        match
          a.duration
        with
        | None -> ""
        | Some time -> (string_of_float time)
      end
  in
  ()

  let close_logger logger  =
    Loggers.close_logger logger

  let flush_logger logger =
    Loggers.flush_logger logger

type log_info =
  {
    global_time: float;
    step_time: float;
    current_task: step list;
    next_depth: int;
  }


let is_dummy step_kind =
  match
    step_kind
  with
  | Dummy -> true
  | Initialisation
  | Cloud_synchronization
  | Extract_gps_data_base
  | Extract_gps_file _
  | Extract_gps_file_from_handmade_files _
  | Extract_gps_file_from_backup_files _
  | Extract_gps_file_from_database _
  | Collect_picture _
  | Collect_picture_from_url _
  | Collect_record_from_url _
  | Patch_gps_file _
  | Build_keywords_automaton
  | Export_transcript _
  | Collect_additional_courses
  | Collect_scholarships
  | Collect_mentoring
  | Collect_minors
  | Collect_majors
  | Collect_departement
  | Collect_dens_candidates
  | Collect_program
  | Collect_cursus_exceptions
  | Collect_decisions
  | Collect_dispenses
  | Collect_compensations
  | Collect_admissions
  | Collect_inscriptions
  | Collect_cursus
  | Collect_course_exceptions
  | Collect_modified_grade
  | Collect_course_name_translations
  | Collect_course_entries
  | Collect_sorted_courses 
  | Dump_missing_grades
  | Dump_missing_ects_attributions
  | Dump_missing_mentors
  | Dump_missing_internship_descriptions
  | Dump_ambiguous_internship_descriptions
  | Dump_mentor_list
  | Dump_national_diploma_list
  | Dump_dens_result
  | Dump_student_list
  | Dump_course_entries
  | Dump_courses_to_be_sorted
  | Dump_missing_course_translation
  | Dump_mineures_suggestions
  | Dump_dens_candidate_suggestions
  | Scan_csv_files _
    -> false

let open_event
    logger prefix ~safe_mode error_handler step_kind log_info =
  if is_dummy step_kind
  then
    let error_handler,() =
      Exception.warn
        logger prefix ~safe_mode error_handler
        __POS__
        ~message:"Inconsistent profiling information, open_event should not be called with a dummy event"
        (Failure "Dummy event in open_event") ()
    in
    error_handler,log_info
  else
    let next_depth = log_info.next_depth in
    let task =
      {
        tag = step_kind ;
        time_start = Sys.time () ;
        duration = None ;
      }
    in
    let _ = Loggers.open_row logger in
    let _ = Loggers.print_cell logger "Start" in
    let terminated_task =
      (task,List.rev_map (fun x -> x.tag) (List.rev log_info.current_task))
    in
    let _ = print_task logger terminated_task in
    let _ = Loggers.close_row logger in
    let _ = flush_logger logger in
    let current_task = task::log_info.current_task in
    error_handler,
    { log_info
      with
        next_depth = next_depth + 1 ;
        current_task = current_task
    }

let close_event logger prefix ~safe_mode error_handler step_kind log_info =
  if is_dummy step_kind then
    let error,() =
      Exception.warn
        logger prefix ~safe_mode error_handler __POS__
        ~message:"Inconsistent profiling information, close_event should not be called with a dummy event"
        (Failure "Dummy event in close_event") ()
    in
    error,log_info
  else
    let rec aux log_info error_handler interrupted =
      let next_depth = log_info.next_depth in
      let error,() =
        if next_depth = 1
        then
          Exception.warn
            logger prefix ~safe_mode error_handler __POS__
            ~message:"Inconsistent profiling information, depth should not be equal to 1 when closing an event"
            (Failure "Depth=1 in close_event") ()
        else
          error_handler,()
      in
      match
        log_info.current_task
      with
      | [] ->
        let message =
          Format.sprintf
            "Inconsistent profiling information, no current task when closing the event (%s)"
            (string_of_step_kind step_kind)
        in
        Exception.warn
          logger prefix ~safe_mode error_handler __POS__
          ~message
          (Failure "No current tasks in close_event") log_info
      | current_task::tail when current_task.tag = step_kind ->
        begin
          let time = Sys.time () -. current_task.time_start in
          let task =
            {
              current_task
              with
                duration = Some time
            }
          in
          let terminated_task =
            (task,List.rev_map (fun x -> x.tag) (List.rev tail))
          in
          let () = Loggers.open_row logger in
          let () = Loggers.print_cell logger
              (if interrupted then "Interrupted"
               else "End") in
          let () = print_task logger terminated_task in
          let () = Loggers.close_row logger in
          let () = flush_logger logger in
          error,
          {
            log_info
            with
              next_depth = next_depth - 1;
              current_task = tail ;
          }
        end
      | current_task::tail ->
        let () = Loggers.open_row logger in
        let terminated_task =
          (current_task,List.rev_map (fun x -> x.tag) (List.rev tail))
        in
        let () = Loggers.print_cell logger "Interrupted" in
        let () = print_task logger terminated_task in
        let () = Loggers.close_row logger in
        let () = flush_logger logger in
        aux
          {
            log_info
            with
              next_depth = next_depth - 1;
              current_task = tail ;
          }
          error_handler true
           in aux log_info error_handler false

let gen_opt gen logger prefix ~safe_mode error_handler step_kind log_info =
  match
    step_kind
  with
  | None -> error_handler,log_info
  | Some e -> gen logger prefix ~safe_mode error_handler e log_info

let open_event_opt = gen_opt open_event
let close_event_opt = gen_opt close_event

let init_log_info () =
  let time = Sys.time () in
  {
    next_depth = 1;
    global_time = time ;
    step_time = time;
    current_task = [];
  }

(*
  let log_info_to_json log_info =
    `Assoc
      [
        "global_time", `Float log_info.global_time ;
        "step_time", `Float log_info.step_time;
      ]

  let log_info_of_json x =
    let init = init_log_info () in
    match x with
    | `Assoc l when List.length l = 2 ->
           begin
             try
               {init with
                global_time =
                  Yojson.Basic.Util.to_float
                    (List.assoc "global_time" l);
                step_time =
                  Yojson.Basic.Util.to_float
                     (List.assoc "step_time" l);
               }
             with
             | Not_found ->
               raise
                 (Yojson.Basic.Util.Type_error ("Not a correct log_info",x))
           end
         | x -> raise
                  (Yojson.Basic.Util.Type_error ("Not a correct log_info",x))
*)

let reset_log log =
  let time = Sys.time () in
  { log
    with
      step_time = time}


let ellapsed_time log =
  let time = Sys.time () in
  time -. log.step_time

let ellapsed_global_time log =
  let time = Sys.time () in
  time -. log.global_time

let set_time log =
  { log with step_time = Sys.time ()}

let dump_log logger log_info =
  let () = Loggers.fprintf logger "/*" in
  let () = Loggers.print_newline logger in
  let () = Loggers.fprintf logger "Profiling" in
  let () = Loggers.print_newline logger in
  let () = Loggers.fprintf logger "Ellapsed_time:                  %f" (ellapsed_global_time log_info) in
  let () = Loggers.print_newline logger in
  let () = Loggers.fprintf logger "*/" in
  let () = Loggers.print_newline logger in
  ()
