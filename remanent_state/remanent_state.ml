type parameters =
  {
    safe_mode: bool;
    cloud_synchronization_mode: Public_data.cloud_synchronization_mode ;
    cloud_client: Public_data.cloud_client ;
    cloud_client_options: string ;
    cloud_repository: string ;
    cloud_support_dynamic_link : bool ;
    pdfgenerator : Public_data.pdf_generator ;
    pdfgenerator_options : string ;
    local_repository: string ;
    distant_repository: string ;
    machine_to_access_gps: string ;
    port_to_access_gps: string;
    repository_to_access_gps: string;
    repository_to_dump_gps_files: string;
    repository_for_handmade_gps_files: string;
    output_alias_repository: string;
    store_gps_file_according_to_their_promotions: bool;
    indicate_promotions_in_gps_file_names: bool;
    repository_to_access_pictures: string;
    pictures_stored_according_to_promotions: bool;
    picture_file_names_mention_promotion: bool;
    file_retriever: Public_data.file_retriever ;
    file_retriever_options: string ;
    file_retriever_log_repository: string ;
    file_retriever_log_file: string;
    file_retriever_time_out_in_seconds: int option;
    file_retriever_checking_period_in_seconds : int;
    profiling_log_file_repository: string;
    profiling_log_file: string;
    error_log_repository: string;
    error_log_file: string;
    date: string;
    comma_symbol: char;
    current_academic_year: Public_data.annee;
    target: string option;
  }



let parameters =
  {
    safe_mode = true;
    cloud_synchronization_mode = Public_data.CommandLine ;
    cloud_client = Public_data.NextCloudCmd ;
    cloud_client_options = "-n --silent" ;
    cloud_repository = "/users/absint3/feret/Nextcloud" ;
    cloud_support_dynamic_link = false ;
    pdfgenerator = Public_data.PdfLatex ;
    pdfgenerator_options = "-interaction=nonstopmode";
    local_repository = "di/suivi_pedagogique" ;
    distant_repository = "https://cloud.di.ens.fr/" ;
    machine_to_access_gps = "violette.ens.fr" ;
    port_to_access_gps = "8080";
    repository_to_access_gps = "gps";
    repository_to_dump_gps_files = "gps_files";
    repository_for_handmade_gps_files = "handmade_gps_files";
    output_alias_repository = "courant";
    store_gps_file_according_to_their_promotions = true;
    indicate_promotions_in_gps_file_names = true;
    repository_to_access_pictures = "pictures";
    pictures_stored_according_to_promotions = true ;
    picture_file_names_mention_promotion = false;
    file_retriever  = Public_data.WGET ;
    file_retriever_options = "" ;
    file_retriever_log_repository = "/users/absint3/feret/tmp" ;
    file_retriever_log_file = "gps_access.log";
    file_retriever_time_out_in_seconds = Some 300;
    file_retriever_checking_period_in_seconds = 5;
    profiling_log_file_repository = "/users/absint3/feret/tmp";
    profiling_log_file = "profiling.html";
    error_log_repository = "/users/absint3/feret/tmp";
    error_log_file = "error.txt";
    date = Tools.date ();
    comma_symbol = ',';
    current_academic_year = "2019";
    target = None ;
  }

type data =
  {
    students: Public_data.student_id list ;
    output_alias: (string * string) option ;
    scholarships: Scholarships.t;
    mentoring: Mentoring.t;
    dpts: Departments.t;
    programs: Programs.t;
    cursus: Cursus.t;
    cursus_exceptions: Cursus_exception.t;
    decisions: Decisions.t;
    admissions: Admissions.t;
    compensations: Compensations.t;
    dispenses: Dispenses.t;
  }

let empty_data =
  {
    students = [];
    scholarships = Scholarships.empty;
    mentoring = Mentoring.empty;
    dpts =  Departments.empty;
    cursus = Cursus.empty;
    programs = Programs.empty;
    output_alias = None;
    cursus_exceptions = Cursus_exception.empty;
    decisions = Decisions.empty;
    admissions = Admissions.empty;
    compensations = Compensations.empty;
    dispenses = Dispenses.empty;
  }

type t =
  {
    parameters : parameters ;
    prefix : string ;
    error_log : Exception.method_handler ;
    profiling_info : Profiling.log_info option ;
    std_logger : Loggers.t option ;
    profiling_logger: Loggers.t option ;
    error_logger: Loggers.t option ;
    data: data;
  }

let get_logger_gen access t =
  t,
  match access t with
  | None -> Loggers.devnul
  | Some a -> a

let get_error_logger t =
  get_logger_gen (fun x -> x.error_logger) t

let get_is_in_safe_mode t =
  t, t.parameters.safe_mode

let get_prefix t = t, t.prefix

let get_cloud_synchronization_mode t =
  t, t.parameters.cloud_synchronization_mode


let get_cloud_client t =
  t, t.parameters.cloud_client

let get_cloud_repository t =
  t,t.parameters.cloud_repository

let get_cloud_support_dynamic_link t =
  t,t.parameters.cloud_support_dynamic_link

let get_pdfgenerator_engine t =
  t,t.parameters.pdfgenerator

let get_pdfgenerator_options t =
  t,t.parameters.pdfgenerator_options
let get_local_repository t =
  let t, cloud =
    get_cloud_repository t
  in
  let local = t.parameters.local_repository in
  t,Printf.sprintf "%s/%s" cloud local

let get_distant_repository t =
  t,t.parameters.distant_repository

let get_cloud_client_options t =
  t,t.parameters.cloud_client_options

let get_file_retriever t =
  t, t.parameters.file_retriever

let get_file_retriever_options t =
  t,
  t.parameters.file_retriever_options

let get_file_retriever_log_repository t =
  t,
  t.parameters.file_retriever_log_repository

let get_file_retriever_log_file t =
  t,
  t.parameters.file_retriever_log_file

let get_file_retriever_time_out_in_second t =
  t, t.parameters.file_retriever_time_out_in_seconds

let get_file_retriever_checking_period t =
  t, t.parameters.file_retriever_checking_period_in_seconds

let get_machine_to_access_gps t =
  t, t.parameters.machine_to_access_gps

let get_port_to_access_gps t =
  t, t.parameters.port_to_access_gps

let get_repository_to_access_gps t =
  t, t.parameters.repository_to_access_gps

let get_repository_to_dump_gps_files t =
  t, t.parameters.repository_to_dump_gps_files

let get_repository_for_handmade_gps_files t =
  t, t.parameters.repository_for_handmade_gps_files
let get_output_alias_repository t =
  t, t.parameters.output_alias_repository

let get_output_alias t =
  t, t.data.output_alias

let set_output_alias t output_alias =
  let output_alias = Some output_alias in
  let data = {t.data with output_alias} in
  {t with data }

let get_store_gps_files_according_to_their_promotions t =
  t,
  t.parameters.store_gps_file_according_to_their_promotions

let get_indicate_promotions_in_gps_file_names t =
  t,
  t.parameters.indicate_promotions_in_gps_file_names

let get_rep_gen get_prefix t =
  let t, main = get_local_repository t in
  let t, repository = get_prefix t in
  t, Printf.sprintf "%s/%s" main repository

let get_students_list_prefix t =
  t, "etudiants"

let get_students_list_repository t =
  get_rep_gen get_students_list_prefix t

let get_scholarships_list_prefix t =
  t, "bourses"

let get_scholarships_list_repository t =
  get_rep_gen get_scholarships_list_prefix t

let get_monitoring_list_prefix t =
    t, "tuteurs"

let get_monitoring_list_repository t =
  get_rep_gen get_monitoring_list_prefix t


let get_departments_list_prefix t =
       t, "departements"

let get_departments_list_repository t =
  get_rep_gen get_departments_list_prefix t

let get_cursus_list_prefix t =
  t, "cursus"

let get_cursus_list_repository t =
  get_rep_gen get_cursus_list_prefix t

let get_programs_list_prefix t =
  t, "diplomes"

let get_programs_list_repository t =
  get_rep_gen get_programs_list_prefix t

let get_cursus_exceptions_list_prefix t =
  t, "exceptions_cursus"

let get_cursus_exceptions_list_repository t =
  get_rep_gen get_cursus_exceptions_list_prefix t

let get_decisions_list_prefix t =
  t, "decisions"

let get_decisions_list_repository t =
  get_rep_gen get_decisions_list_prefix t

let get_admissions_list_prefix t =
  t, "admissions"

let get_admissions_list_repository t =
  get_rep_gen get_admissions_list_prefix t

let get_compensations_list_prefix t =
    t, "compensations"

let get_compensations_list_repository t =
  get_rep_gen get_compensations_list_prefix t

let get_dispenses_list_prefix t =
  t, "dispenses"

let get_dispenses_list_repository t =
      get_rep_gen get_dispenses_list_prefix t

let get_csv_separator t = t, Some ','

let get_std_logger t =
  get_logger_gen (fun x -> x.std_logger) t

let get_error_handler t =
  t, t.error_log

let set_error_handler error_log t =
  {t with error_log}

let get_profiler_logger t =
  get_logger_gen (fun x -> x.profiling_logger) t

let get_profiling_info t =
  t, t.profiling_info

let set_profiling_info profiling_info t =
  let profiling_info = Some profiling_info in
  {t with profiling_info}

let gen_profiler f step_kind_opt t =
  let t, logger = get_profiler_logger t in
  let t, prefix = get_prefix t in
  let t, safe_mode = get_is_in_safe_mode t in
  let t, error_handler = get_error_handler t in
  let t, profiling_info_opt = get_profiling_info t in
  match profiling_info_opt with
  | None -> t
  | Some profiling_info ->
    let error_handler, profiling_info =
      f logger prefix ~safe_mode error_handler
        step_kind_opt profiling_info in
    let t = set_error_handler error_handler t in
    let t = set_profiling_info profiling_info t in
    t

let open_event_opt step_kind_opt t =
  gen_profiler Profiling.open_event_opt step_kind_opt t

let close_event_opt step_kind_opt t =
  gen_profiler Profiling.close_event_opt step_kind_opt t

let get_option parameters =
  let target =
    if Array.length Sys.argv > 1
    then Some Sys.argv.(1)
    else None
  in
  {parameters with target}

let get_option_quick parameters =
  let target =
    if Array.length Sys.argv > 1
    then Some Sys.argv.(1)
    else None
  in
  {parameters with target}

let get_option state =
  let state =
    open_event_opt
      (Some Profiling.Initialisation)
      state
  in
  let parameters = get_option state.parameters in
  let state =
    close_event_opt
      (Some Profiling.Initialisation)
      state
  in
  {state with parameters}

let init () =
  let profiling_info =
    Some (Profiling.init_log_info ())
  in
  let std_logger =
    Some
      (Loggers.open_logger_from_formatter Format.std_formatter)
  in
  let error_log =
    Exception.empty_error_handler
  in
  let parameters =
    get_option_quick parameters
  in
  let rep = parameters.error_log_repository in
  let file = parameters.error_log_file in
  let error_logger =
    let fic = open_out (Printf.sprintf "%s/%s" rep file) in
    Some
      (Loggers.open_logger_from_channel
         ~mode:Loggers.TXT
         fic)
  in
  let rep = parameters.profiling_log_file_repository in
  let file = parameters.profiling_log_file in
  let profiling_logger =
    let fic = open_out (Printf.sprintf "%s/%s" rep file) in
    Some
      (Loggers.open_logger_from_channel
         ~mode:Loggers.HTML_Tabular
         fic)
  in
  let data = empty_data in
  let prefix = "" in
  let state =
    {
      parameters ;
      prefix ;
      error_log ;
      profiling_info  ;
      std_logger ;
      profiling_logger ;
      error_logger ;
      data ;
    }
  in
  let state = get_option state in
  state

let warn_dft pos message exn default t =
  let t,error_handler = get_error_handler t in
  let t, log = get_std_logger t in
  let t, prefix = get_prefix t in
  let t, safe_mode = get_is_in_safe_mode t in
  let error_handler, output =
    Exception.warn
      log prefix ~safe_mode error_handler pos ~message exn default
  in
  set_error_handler error_handler t, output

let warn pos message exn t =
  fst (warn_dft pos message exn () t)

let stop pos message exn t =
  let t = warn pos message exn t in
  let () = exit 1 in
  t

let which_logger_gen ?logger f t =
  match logger with
  | Some a -> a
  | None -> snd (f t)

let which_logger ?logger t =
  match logger with
  | Some a -> a
  | None -> snd (get_std_logger t)


let fprintf ?logger t x =
  Loggers.fprintf
    (which_logger ?logger t)
    x

let breakpage ?logger t =
  Loggers.breakpage (which_logger ?logger t)

let log ?logger ?backgroundcolor ?textcolor ?lineproportion t x =
  Loggers.log
    (which_logger ?logger t)
    ?backgroundcolor ?textcolor ?lineproportion
    x

let log_string ?logger ?backgroundcolor ?textcolor ?lineproportion t x =
  let mode =
    Loggers.encapsulate
      (Loggers.get_encoding_format (which_logger ?logger t))
  in
  let b = Buffer.create 0 in
  let fmt_buffer = Format.formatter_of_buffer b in
  let logger = Loggers.open_logger_from_formatter ~mode fmt_buffer in
  let () =
    log
      ~logger
      ?backgroundcolor
      ?textcolor
      ?lineproportion
      t "%s" x
  in
  let () = Format.pp_print_flush fmt_buffer () in
  Buffer.contents b

let set_std_logger t logger =
  let std_logger = Some logger in
  {t with std_logger}

let open_array pos ?logger ~with_lines ?size ?color ?bgcolor ?align ~title t =
  let t,logger =
    match logger with
    | None ->
      let logger = which_logger ?logger t in
      let logger =
        if with_lines
        then
          Loggers.with_lines logger
        else
          logger
      in
      let t = set_std_logger t logger in
      t, logger
    | Some logger ->
      let logger =
        if with_lines
        then
          Loggers.with_lines logger
        else
          logger
      in
      t, logger
  in
  let error = Loggers.open_array ?size ?color ?bgcolor ?align ~title logger in
  if error
  then
    warn
      pos
      "arguments of open_array shall have the same length"
      Exit
      t
  else
    t

let close_array ?logger t =
  let () = Loggers.close_array (which_logger ?logger t) in
  let _ =
    match logger with
    | Some _ -> t
    | None ->
      let logger = (which_logger ?logger t) in
      let logger = Loggers.without_lines logger in
      set_std_logger t logger
  in
  ()

let open_row ?logger ?macro t =
  Loggers.open_row ?macro (which_logger ?logger t)
let close_row ?logger t =
  Loggers.close_row (which_logger ?logger t)

let print_cell ?logger s t =
  Loggers.print_cell (which_logger ?logger t) s

let print_optional_cell ?logger s t =
  Loggers.print_optional_cell (which_logger ?logger t) s


let flush ?logger t =
  Loggers.flush_logger (which_logger ?logger t)

let print_newline ?logger t =
  Loggers.print_newline (which_logger ?logger t)

let print_errors ?logger prefix t =
  let t,error_handler = get_error_handler t in
  let () =
    Exception.print
      (which_logger_gen get_error_logger ?logger t) prefix error_handler
  in
  t

type save_logger = Loggers.t option
let save_std_logger t = t.std_logger
let restore_std_logger t std_logger = {t with std_logger}

let get_launching_date t =
  t,Tools.date ()

let get_comma_symbol t =
  t,t.parameters.comma_symbol

let std_logger =
  Loggers.open_logger_from_formatter (Format.std_formatter)

let close_logger ?logger t =
  let log = which_logger ?logger t in
  let () = Loggers.close_logger log in
  let t,log' = get_std_logger t in
  if log == log'
  then
    {t with std_logger=None}
  else
    t



let get_data t = t.data
let set_data data t = {t with data}
let lift_get get t = get (get_data t)
let lift_set set data t =
  set_data (set data (get_data t)) t

let get_students data = data.students
let get_students t = lift_get get_students t
let set_students students data = {data with students}
let set_students students t = lift_set set_students students t

let get_scholarships data = data.scholarships
let get_scholarships t = lift_get get_scholarships t
let set_scholarships scholarships data = {data with scholarships}
let set_scholarships scholarships t =
  lift_set set_scholarships scholarships t

let get_mentoring data = data.mentoring
let get_mentoring t = lift_get get_mentoring t
let set_mentoring mentoring data = {data with mentoring}
let set_mentoring mentoring t =
  lift_set set_mentoring mentoring t

let get_cursus data = data.cursus
let get_cursus t = lift_get get_cursus t
let set_cursus cursus data = {data with cursus}
let set_cursus cursus t =
  lift_set set_cursus cursus t

let get_dpts data = data.dpts
let get_dpts t = lift_get get_dpts t
let set_dpts dpts data = {data with dpts}
let set_dpts dpts t =
  lift_set set_dpts dpts t

let get_programs data = data.programs
let get_programs t = lift_get get_programs t
let set_programs programs data = {data with programs}
let set_programs programs t =
    lift_set set_programs programs t

let get_cursus_exceptions data = data.cursus_exceptions
let get_cursus_exceptions t = lift_get get_cursus_exceptions t
let set_cursus_exceptions cursus_exceptions data =
  {data with cursus_exceptions}
let set_cursus_exceptions cursus_exceptions t =
  lift_set set_cursus_exceptions cursus_exceptions t

let get_compensations data = data.compensations
let get_compensations t = lift_get get_compensations t
let set_compensations compensations data =
  {data with compensations}
let set_compensations compensations t =
  lift_set set_compensations compensations t

let get_decisions data = data.decisions
let get_decisions t = lift_get get_decisions t
let set_decisions decisions data =
  {data with decisions}
let set_decisions decisions t =
  lift_set set_decisions decisions t

let get_admissions data = data.admissions
let get_admissions t = lift_get get_admissions t
let set_admissions admissions data =
  {data with admissions}
let set_admissions admissions t =
    lift_set set_admissions admissions t

let get_dispenses data = data.dispenses
let get_dispenses t = lift_get get_dispenses t
let set_dispenses dispenses data =
  {data with dispenses}
let set_dispenses dispenses t =
  lift_set set_dispenses dispenses t

let add_gen get set add pos data t =
  let t, acc =
    add
      pos
      t
      data (get t)
  in
  set acc t

let add_student _unify =
  add_gen
    get_students
    set_students
    (fun _ t a b -> t, a::b)

let add_scholarship unify =
  add_gen
    get_scholarships
    set_scholarships
    (Scholarships.add_scholarship unify)

let get_scholarship ~firstname ~lastname t =
  let scholarship_opt =
    Scholarships.get_scholarship ~firstname ~lastname t.data.scholarships
  in
  t, scholarship_opt

let add_mentoring unify =
  add_gen
    get_mentoring
    set_mentoring
    (Mentoring.add_mentoring unify)

let get_mentoring ~firstname ~lastname ~year ?tuteur_gps pos t =
    let mentoring_opt =
      Mentoring.get_mentoring
        ~firstname ~lastname ~year (get_mentoring t)
    in
    match mentoring_opt with
    | Some a ->
      t, Some a
    | None ->
      let msg =
        Format.sprintf
          "Pas de tuteur pour %s %s en %s dans les fichiers du département"
          firstname lastname year
      in
      warn_dft
        pos
        msg
        Exit
        tuteur_gps
        t

let add_cursus unify =
  add_gen
    get_cursus
    set_cursus
    (Cursus.add_cursus unify)

let get_cursus ~year ~level ?dpt pos t =
  let cursus_opt =
    Cursus.get_cursus
       ~year ~level ?dpt (get_cursus t)
  in
  match cursus_opt with
  | Some a ->
    t, Some a
  | None ->
    let msg =
      Format.sprintf
        "Pas de cursus pour %s%s en %s dans les fichiers du département"
        level
        (match dpt with
         | None -> "" | Some i -> Format.sprintf " %s" i)
        year
    in
    warn
      pos
      msg
      Exit
      t, None


let add_dpt unify =
  add_gen
    get_dpts
    set_dpts
    (Departments.add_dpt unify)

let get_dpt ~acronym t =
  let dpt_opt =
    Departments.get_dpt ~acronym t.data.dpts
  in
  t, dpt_opt

let add_program unify =
  add_gen
    get_programs
    set_programs
    (Programs.add_program unify)

let get_program ~code_gps t =
  let program_opt =
    Programs.get_program ~code_gps  t.data.programs
  in
  t, program_opt

let add_dispense unify =
    add_gen
      get_dispenses
      set_dispenses
      (Dispenses.add_dispense warn unify)

let get_dispenses
    ?firstname ?lastname
    ?year
    ?program
    ?dpt
    t =
    let dispense_opt =
      Dispenses.get_dispenses
        ?firstname ?lastname
        ?year
        ?program
        ?dpt
        t.data.dispenses
    in
    t, dispense_opt

let add_decision unify =
  add_gen
    get_decisions
    set_decisions
    (Decisions.add_decision unify)

let get_decision
    ~firstname ~lastname ~year
    ~program ~dpt t =
  let decision_opt =
    Decisions.get_decision
      ~firstname ~lastname ~year
      ~program ~dpt
      t.data.decisions
  in
  t, decision_opt

let add_admission unify =
  add_gen
    get_admissions
    set_admissions
    (Admissions.add_admission unify)

let get_admission
    ~firstname ~lastname ~year
     t =
  let admission_opt =
    Admissions.get_admission
      ~firstname ~lastname ~year
      t.data.admissions
  in
  t, admission_opt

let add_compensation unify =
  add_gen
    get_compensations
    set_compensations
    (Compensations.add_compensation unify)

let get_compensation
    ~firstname ~lastname ~year ~codecours t =
  let compensation_opt =
    Compensations.get_compensation
      ~firstname ~lastname ~year ~codecours
      t.data.compensations
  in
  t, compensation_opt

let add_cursus_exception unify =
  add_gen
    get_cursus_exceptions
    set_cursus_exceptions
    (Cursus_exception.add_cursus_exception unify)

let get_cursus_exception
    ~firstname ~lastname ~year ~code_gps t =
  let exception_opt =
    Cursus_exception.get_cursus_exception
      ~firstname ~lastname ~year ~code_gps t.data.cursus_exceptions
  in
  t, exception_opt

let get_current_academic_year t =
  t, t.parameters.current_academic_year

let get_pictures_prefix t =
  t, t.parameters.repository_to_access_pictures

let get_picture_file_names_mention_promotion t =
  t, t.parameters.picture_file_names_mention_promotion

let get_pictures_stored_according_to_promotions t =
  t, t.parameters.pictures_stored_according_to_promotions

let get_pictures_repository t =
  let t, main = get_local_repository t in
  let t, repository = get_pictures_prefix t in
  t, Printf.sprintf "%s/%s" main repository

let get_picture_potential_locations
    ~firstname ~lastname ~year t =
  let t, rep = get_pictures_repository t in
  let t, b =
    get_pictures_stored_according_to_promotions t
  in
  let rep =
    if b then
      if rep = "" then year
      else
        Format.sprintf "%s/%s" rep year
    else
      rep
  in
  let t,b =
    get_picture_file_names_mention_promotion t
  in
  let yearprefix =
    if b then year else ""
  in
  let filename =
    Printf.sprintf "%s%s%s"
      yearprefix
      (Special_char.uppercase
         (Special_char.correct_string_filename lastname))
      (Special_char.lowercase
         (Special_char.correct_string_filename firstname))
  in
  let base =
    if rep = ""
    then filename
    else
      Printf.sprintf "%s/%s" rep filename
  in
  t, [base^".jpg";base^".pdf"]

let get_target t = t, t.parameters.target

let list_all_cursus state =
  let () =
    Public_data.LevelMap.iter
      (fun level ->
         Public_data.DptOptMap.iter
           (fun dpt_opt ->
              Public_data.YearMap.iter
                (fun year cursus ->
                   Format.printf
                     "%s %s %s -> %s %s %s @ "
                     level
                     (match dpt_opt with
                      | None -> "None"
                      | Some a -> a
                     )
                     year
                     cursus.Public_data.cursus_niveau
                     (match cursus.Public_data.cursus_dpt with
                      | None -> "None"
                      | Some a -> a
                     )
                     cursus.Public_data.cursus_annee_academique
              )))
      state.data.cursus
  in
  Format.print_flush ()
