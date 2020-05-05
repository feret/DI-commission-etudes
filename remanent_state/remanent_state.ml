type parameters =
  {
    safe_mode: bool;
    cloud_synchronization_mode: Public_data.cloud_synchronization_mode ;
    cloud_client: Public_data.cloud_client ;
    cloud_client_options: string ;
    cloud_repository: string ;
    local_repository: string ;
    distant_repository: string ;
    machine_to_access_gps: string ;
    port_to_access_gps: string;
    repository_to_access_gps: string;
    repository_to_dump_gps_files: string;
    store_gps_file_according_to_their_promotions: bool;
    indicate_promotions_in_gps_file_names: bool;
    file_retriever: Public_data.file_retriever ;
    file_retriever_options: string ;
    file_retriever_log_repository: string ;
    file_retriever_log_file: string;
    file_retriever_time_out_in_seconds: int option;
    file_retriever_checking_period_in_seconds : int;
    profiling_log_file_repository: string ;
    profiling_log_file: string}


let parameters =
  {
    safe_mode = true;
    cloud_synchronization_mode = Public_data.CommandLine ;
    cloud_client = Public_data.NextCloudCmd ;
    cloud_client_options = "-n" ;
    cloud_repository = "/users/absint3/feret/Nextcloud" ;
    local_repository = "di/direction_des_etudes" ;
    distant_repository = "https://cloud.di.ens.fr/" ;
    machine_to_access_gps = "violette.ens.fr" ;
    port_to_access_gps = "8080";
    repository_to_access_gps = "gps";
    repository_to_dump_gps_files = "gps_files";
    store_gps_file_according_to_their_promotions = true;
    indicate_promotions_in_gps_file_names = true;
    file_retriever  = Public_data.WGET ;
    file_retriever_options = "" ;
    file_retriever_log_repository = "/users/absint3/feret/tmp" ;
    file_retriever_log_file = "gps_access.log";
    file_retriever_time_out_in_seconds = Some 300;
    file_retriever_checking_period_in_seconds = 5;
    profiling_log_file_repository = "/users/absint3/feret/tmp";
    profiling_log_file = "profiling.html"

  }

type t =
  {
    parameters : parameters ;
    prefix : string ;
    error_log : Exception.method_handler ;
    profiling_info : Profiling.log_info option ;
    std_logger : Loggers.t option ;
    profiling_logger: Loggers.t option ;
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

  let prefix = "" in
  let state =
    {
      parameters ;
      prefix ;
      error_log ;
      profiling_info  ;
      std_logger ;
      profiling_logger ;
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



let log t x =
  Loggers.fprintf (snd (get_std_logger t)) x
let flush t =
  Loggers.flush_logger (snd (get_std_logger t))
