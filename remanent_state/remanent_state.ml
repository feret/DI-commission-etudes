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
    output_alias_repository: string;
    store_gps_file_according_to_their_promotions: bool;
    indicate_promotions_in_gps_file_names: bool;
    file_retriever: Public_data.file_retriever ;
    file_retriever_options: string ;
    file_retriever_log_repository: string ;
    file_retriever_log_file: string;
    file_retriever_time_out_in_seconds: int option;
    file_retriever_checking_period_in_seconds : int;
    profiling_log_file_repository: string ;
    profiling_log_file: string;
    date: string;
    comma_symbol: char;
    current_academic_year: Public_data.annee;
  }



let parameters =
  {
    safe_mode = true;
    cloud_synchronization_mode = Public_data.CommandLine ;
    cloud_client = Public_data.NextCloudCmd ;
    cloud_client_options = "-n" ;
    cloud_repository = "/users/absint3/feret/Nextcloud" ;
    cloud_support_dynamic_link = false ;
    pdfgenerator = Public_data.PdfLatex ;
    pdfgenerator_options = "-interaction=nonstopmode";
    local_repository = "di/direction_des_etudes" ;
    distant_repository = "https://cloud.di.ens.fr/" ;
    machine_to_access_gps = "violette.ens.fr" ;
    port_to_access_gps = "8080";
    repository_to_access_gps = "gps";
    repository_to_dump_gps_files = "gps_files";
    output_alias_repository = "courant";
    store_gps_file_according_to_their_promotions = true;
    indicate_promotions_in_gps_file_names = true;
    file_retriever  = Public_data.WGET ;
    file_retriever_options = "" ;
    file_retriever_log_repository = "/users/absint3/feret/tmp" ;
    file_retriever_log_file = "gps_access.log";
    file_retriever_time_out_in_seconds = Some 300;
    file_retriever_checking_period_in_seconds = 5;
    profiling_log_file_repository = "/users/absint3/feret/tmp";
    profiling_log_file = "profiling.html";
    date = Tools.date ();
    comma_symbol = ',';
    current_academic_year = "2019";
  }

type data =
  {
    output_alias: (string * string) option ;
    scholarships: Scholarships.t;
    mentoring: Mentoring.t;
  }

let empty_data =
  {
    scholarships = Scholarships.empty;
    mentoring = Mentoring.empty;
    output_alias = None
  }

type t =
  {
    parameters : parameters ;
    prefix : string ;
    error_log : Exception.method_handler ;
    profiling_info : Profiling.log_info option ;
    std_logger : Loggers.t option ;
    profiling_logger: Loggers.t option ;
    data: data;
  }

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

let get_students_list_prefix t =
  t, "etudiants"

let get_students_list_repository t =
  let t, main = get_local_repository t in
  let t, repository = get_students_list_prefix t in
  t, Printf.sprintf "%s/%s" main repository

let get_scholarships_list_prefix t =
  t, "bourses"

let get_scholarships_list_repository t =
  let t, main = get_local_repository t in
  let t, repository = get_scholarships_list_prefix t in
  t, Printf.sprintf "%s/%s" main repository

let get_monitoring_list_prefix t =
    t, "tuteurs"

let get_monitoring_list_repository t =
    let t, main = get_local_repository t in
    let t, repository = get_monitoring_list_prefix t in
    t, Printf.sprintf "%s/%s" main repository

let get_csv_separator t = t, Some ','
let get_logger_gen access t =
  t,
  match access t with
  | None -> Loggers.devnul
  | Some a -> a

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

let get_option parameters = parameters
let get_option_quick parameters = parameters

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

let flush ?logger t =
  Loggers.flush_logger (which_logger ?logger t)

let print_newline ?logger t =
  Loggers.print_newline (which_logger ?logger t)

let print_errors ?logger prefix t =
  let t,error_handler = get_error_handler t in
  let () = Exception.print (which_logger ?logger t) prefix error_handler in
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
let get_scholarships data = data.scholarships
let get_scholarships t = get_scholarships (get_data t)
let set_data data t = {t with data}
let set_scholarships scholarships data =
  {data with scholarships}
let set_scholarships scholarships t =
  set_data (set_scholarships scholarships (get_data t)) t
let get_mentoring data = data.mentoring
let get_mentoring t = get_mentoring (get_data t)
let set_mentoring mentoring data = {data with mentoring}
let set_mentoring mentoring t =
  set_data (set_mentoring mentoring (get_data t)) t

let add_gen get set add pos data t =
  let t, error_handler = get_error_handler t in
  let t, log = get_std_logger t in
  let t, prefix = get_prefix t in
  let t, safe_mode = get_is_in_safe_mode t in
  let error_handler, acc =
    add
      ~safe_mode log prefix pos error_handler
      data (get t)
  in
  let t = set acc t in
  let t = set_error_handler error_handler t in
  t

let add_scholarship =
  add_gen
    get_scholarships
    set_scholarships
    Scholarships.add_scholarship

let get_scholarship ~firstname ~lastname t =
  let scholarship_opt =
    Scholarships.get_scholarship ~firstname ~lastname t.data.scholarships
  in
  t, scholarship_opt

let add_mentoring =
  add_gen
    get_mentoring
    set_mentoring
    Mentoring.add_mentoring

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

let get_current_academic_year t =
  t, t.parameters.current_academic_year
