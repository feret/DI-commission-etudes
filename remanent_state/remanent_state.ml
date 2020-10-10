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
    output_repository: string;
    database_repository: string;
    study_repository:string;
    parameters_repository:string;
    gps_backup_repository:string;
    repository_to_dump_gps_files: string;
    repository_to_dump_gps_server_faillures: string;
    repository_to_dump_attestations: string;
    repository_for_handmade_gps_files: string;
    repository_for_backup_gps_files: string;
    repository_to_dump_issues: string;
    repository_to_dump_reports: string;
    output_alias_repository: string;
    store_output_according_to_their_promotions: bool;
    indicate_promotions_in_gps_file_names: bool;
    indicate_promotions_in_attestation_file_names: bool;
    repository_to_access_pictures: string;
    pictures_stored_according_to_promotions: bool;
    picture_file_names_mention_promotion: bool;
    file_retriever: Public_data.file_retriever ;
    file_retriever_options: string ;
    file_retriever_log_repository: string ;
    file_retriever_log_file: string;
    file_retriever_time_out_in_seconds: int option;
    file_retriever_checking_period_in_seconds : int;
    tmp_profiling_repository: string;
    profiling_log_file_repository: string;
    profiling_log_file: string;
    tmp_error_repository:string ;
    error_log_repository: string;
    error_log_file: string;
    comma_symbol: char;
    current_academic_year: Public_data.annee;
    target: string option;
    repository_for_bourses: string;
    repository_for_tuteurs: string;
    repository_for_cours: string;
    repository_for_departements: string;
    repository_for_cursus: string;
    repository_for_diplomes: string;
    repository_for_cursus_exceptions: string;
    repository_for_decisions: string;
    repository_for_admissions: string;
    repository_for_compensation: string;
    repository_for_dispenses: string;
    repository_for_additional_courses: string;
    repository_to_dump_missing_pictures: string;
    repository_to_dump_non_accepted_grades: string;
    repository_to_dump_non_validated_internships: string;
    repository_to_dump_missing_grades: string;
    repository_to_dump_missing_mentors: string;
    repository_to_dump_missing_ects_attributions: string;
    repository_to_dump_missing_internship_descriptions: string;
    repository_to_dump_ambiguous_internship_descriptions:string;
    repository_to_dump_national_diplomas: string;
    repository_to_dump_dens: string;
    repository_to_dump_mentors: string;
    signature: string;
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
    output_repository = "sortie" ;
    database_repository = "base_de_donnees";
    study_repository = "etudes";
    parameters_repository = "parametres" ;
    gps_backup_repository = "gps_backup" ;
    repository_to_dump_gps_files = "fiches_de_notes";
    repository_to_dump_issues = "problemes";
    repository_to_dump_reports = "rapports";
    repository_to_dump_missing_pictures = "photos_manquantes";
    repository_to_dump_gps_server_faillures = "echecs_extraction_gps";
    repository_to_dump_non_accepted_grades = "notes_non_acceptees" ;
    repository_to_dump_non_validated_internships = "stages_non_acceptes";
    repository_to_dump_attestations = "attestations";
    repository_to_dump_missing_grades = "notes_manquantes";
    repository_to_dump_missing_mentors = "tuteurs_manquants";
    repository_to_dump_missing_ects_attributions = "ects_non_attribuees";
    repository_to_dump_missing_internship_descriptions = "stages_manquants";
    repository_to_dump_ambiguous_internship_descriptions = "stages_ambigus";
    repository_for_bourses = "bourses";
    repository_for_tuteurs = "tuteurs";
    repository_for_cours = "cours";
    repository_for_departements = "departements";
    repository_for_cursus = "cursus";
    repository_for_diplomes = "diplomes";
    repository_for_cursus_exceptions = "exceptions_cursus";
    repository_for_decisions = "decisions";
    repository_for_admissions = "admissions";
    repository_for_compensation = "compensations";
    repository_for_dispenses = "dispenses";
    repository_for_additional_courses = "cours_a_ajouter";
    repository_to_dump_dens = "dens";
    repository_to_dump_national_diplomas = "diplomes_nationaux";
    repository_to_dump_mentors = "tuteurs";
    repository_for_handmade_gps_files = "fichier_gps_fait_a_la_main";
    repository_for_backup_gps_files = "fichier_gps_de_secours";
    output_alias_repository = "courant";
    store_output_according_to_their_promotions = true;
    indicate_promotions_in_gps_file_names = true;
    indicate_promotions_in_attestation_file_names = true;
    repository_to_access_pictures = "photos";
    pictures_stored_according_to_promotions = true ;
    picture_file_names_mention_promotion = false;
    file_retriever  = Public_data.WGET ;
    file_retriever_options = "" ;
    file_retriever_log_repository = "/users/absint3/feret/tmp" ;
    file_retriever_log_file = "gps_access.log";
    file_retriever_time_out_in_seconds = Some 300;
    file_retriever_checking_period_in_seconds = 5;
    tmp_profiling_repository = "/users/absint3/feret/tmp";
    profiling_log_file_repository = "profiling";
    profiling_log_file = "profiling.html";
    tmp_error_repository = "/users/absint3/feret/tmp";
    error_log_repository = "erreurs_internes";
    error_log_file = "error.txt";
    comma_symbol = ',';
    current_academic_year = "2019";
    target = None ;
    signature = "feret+tampon.pdf"
  }

type data =
  {
    students: Public_data.student_id list ;
    output_alias: (string * string) option ;
    scholarships: Scholarships.t;
    mentoring: Mentoring.t;
    course_exceptions: Course_exceptions.t;
    dpts: Departments.t;
    programs: Programs.t;
    cursus: Cursus.t;
    cursus_exceptions: Cursus_exception.t;
    decisions: Decisions.t;
    admissions: Admissions.t;
    compensations: Compensations.t;
    additional_courses: Cours_a_ajouter.t;
    dispenses: Dispenses.t;
    missing_pictures: Public_data.student list;
    gps_server_faillures: Public_data.student list;
    non_accepted_grades: Public_data.missing_grade list;
    non_validated_internships:
      Public_data.missing_internship_description list;
    missing_grades: Public_data.missing_grade list;
    missing_ects_attributions: Public_data.missing_grade list;
    missing_mentors: Public_data.missing_mentor list;
    missing_internship_descriptions: Public_data.missing_internship_description list;
    ambiguous_internship_descriptions: Public_data.missing_internship_description list;
    mentors: Public_data.mentor list;
    national_diplomas: Public_data.diplome_national list;
    dens: Public_data.dens list;
  }

let empty_data =
  {
    students = [];
    scholarships = Scholarships.empty;
    mentoring = Mentoring.empty;
    course_exceptions = Course_exceptions.empty;
    dpts =  Departments.empty;
    cursus = Cursus.empty;
    programs = Programs.empty;
    additional_courses = Cours_a_ajouter.empty;
    output_alias = None;
    cursus_exceptions = Cursus_exception.empty;
    decisions = Decisions.empty;
    admissions = Admissions.empty;
    compensations = Compensations.empty;
    dispenses = Dispenses.empty;
    missing_pictures = [];
    gps_server_faillures = [];
    non_accepted_grades = [];
    missing_grades = [];
    missing_ects_attributions = [];
    missing_mentors = [];
    missing_internship_descriptions = [];
    ambiguous_internship_descriptions = [];
    non_validated_internships = [];
    mentors = [];
    national_diplomas = [];
    dens = [];
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
    date: string;
  }


let get_launching_date t =
  t,t.date

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

let get_signature t =
  let t, rep = get_local_repository t in
  if rep = ""
  then
    t, t.parameters.signature
  else
    t, Format.sprintf "%s/%s" rep t.parameters.signature

let get_repository_to_access_gps t =
  t, t.parameters.repository_to_access_gps

let get_output_repository t =
  let t, local = get_local_repository t in
  match
    local, t.parameters.output_repository
  with
  | "","" -> t, ""
  | "",a | a,""-> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_dated_output_repository t =
  let t, output = get_output_repository t in
  let t, date = get_launching_date t in
  match output with
  | "" -> t, date
  | _ ->
    t, Format.sprintf "%s/%s" output date

let get_repository_to_dump_issues t =
  let t, output =
    get_dated_output_repository t
  in
  match
    output,
    t.parameters.repository_to_dump_issues
  with
  | "","" -> t, ""
  | "",a | a,""-> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_repository_to_dump_missing_gen get t =
  let t, rep = get_repository_to_dump_issues t in
  match rep with
  | "" -> t, get t
  | _ -> t, Format.sprintf "%s/%s" rep (get t)

let get_repository_to_dump_missing_pictures t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_pictures)
    t

let get_repository_to_dump_gps_server_faillures t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_gps_server_faillures)
    t

let get_repository_to_dump_non_validated_internships t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_non_validated_internships)
    t

let get_repository_to_dump_non_accepted_grades t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_non_accepted_grades)
    t


let get_repository_to_dump_missing_grades t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_grades
    )
    t

let get_repository_to_dump_missing_mentors t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_mentors)
    t

let get_repository_to_dump_missing_internship_descriptions t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_internship_descriptions)
    t

let get_repository_to_dump_missing_ects_attributions t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_ects_attributions)
    t

let get_repository_to_dump_ambiguous_internship_descriptions t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_ambiguous_internship_descriptions)
    t

let get_repository_to_dump_reports t =
  let t, output =
    get_dated_output_repository t
  in
  match
    output,
    t.parameters.repository_to_dump_reports
  with
  | "","" -> t, ""
  | "",a | a,""-> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_repository_to_dump_reports_gen get t =
  let t, rep = get_repository_to_dump_reports t in
  match rep with
  | "" -> t, get t
  | _ -> t, Format.sprintf "%s/%s" rep (get t)

let get_repository_to_dump_mentors t =
  get_repository_to_dump_reports_gen
    (fun t ->
       t.parameters.repository_to_dump_mentors)
    t

let get_repository_to_dump_national_diplomas t =
  get_repository_to_dump_reports_gen
    (fun t ->
       t.parameters.repository_to_dump_national_diplomas)
    t

let get_repository_to_dump_dens t =
  get_repository_to_dump_reports_gen
    (fun t ->
       t.parameters.repository_to_dump_dens)
    t

let get_repository_to_dump_gps_files ?output_repository t =
  let t, output =
    match output_repository with
    | Some rep -> t, rep
    | None -> get_dated_output_repository t
  in
  match
    output,
    t.parameters.repository_to_dump_gps_files
  with
  | "","" -> t, ""
  | "",a | a,""-> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_repository_to_dump_attestations t =
  let t, output =
    get_dated_output_repository t
  in
  match
    output,
    t.parameters.repository_to_dump_attestations
  with
  | "","" -> t, ""
  | "",a | a,""-> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b


let get_repository_for_handmade_gps_files t =
  match
    t.parameters.gps_backup_repository,
    t.parameters.repository_for_handmade_gps_files
  with
  | "","" -> t,""
  | "",a | a,"" -> t,a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_repository_for_backup_gps_files t =
  match
    t.parameters.gps_backup_repository,
    t.parameters.repository_for_backup_gps_files
  with
  | "","" -> t,""
  | "",a | a,"" -> t,a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_output_alias_repository t =
  t, t.parameters.output_alias_repository

let get_output_alias t =
  t, t.data.output_alias

let set_output_alias t output_alias =
  let output_alias = Some output_alias in
  let data = {t.data with output_alias} in
  {t with data }

let get_store_output_according_to_their_promotions t =
  t,
  t.parameters.store_output_according_to_their_promotions

let get_indicate_promotions_in_gps_file_names t =
  t,
  t.parameters.indicate_promotions_in_gps_file_names

let get_indicate_promotions_in_attestation_file_names t =
  t,
  t.parameters.indicate_promotions_in_attestation_file_names


let get_rep_gen get_main get_prefix t =
  let t, main = get_main t in
  let t, repository = get_prefix t in
  t, Printf.sprintf "%s/%s" main repository

let get_students_list_prefix t =
  t, "etudiants"

let get_study t =
  let t, local = get_local_repository t in
  match local, t.parameters.study_repository with
  | "", "" -> t, ""
  | a,"" | "",a -> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_bdd t =
  let t, local = get_local_repository t in
  match local, t.parameters.database_repository with
  | "", "" -> t, ""
  | a,"" | "",a -> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_students_list_repository t =
  get_rep_gen get_bdd get_students_list_prefix t

let get_scholarships_list_prefix t =
  t, t.parameters.repository_for_bourses

let get_scholarships_list_repository t =
  get_rep_gen get_bdd get_scholarships_list_prefix t

let get_monitoring_list_prefix t =
    t, t.parameters.repository_for_tuteurs

let get_monitoring_list_repository t =
  get_rep_gen get_bdd get_monitoring_list_prefix t

let get_course_exceptions_list_prefix t =
      t, t.parameters.repository_for_cours

let get_course_exceptions_list_repository t =
  get_rep_gen get_bdd get_course_exceptions_list_prefix t

let get_departments_list_prefix t =
       t, t.parameters.repository_for_departements

let get_departments_list_repository t =
  get_rep_gen get_study get_departments_list_prefix t

let get_cursus_list_prefix t =
  t, t.parameters.repository_for_cursus

let get_cursus_list_repository t =
  get_rep_gen get_study get_cursus_list_prefix t

let get_programs_list_prefix t =
  t, t.parameters.repository_for_diplomes

let get_programs_list_repository t =
  get_rep_gen get_study get_programs_list_prefix t

let get_cursus_exceptions_list_prefix t =
  t, t.parameters.repository_for_cursus_exceptions

let get_cursus_exceptions_list_repository t =
  get_rep_gen get_bdd get_cursus_exceptions_list_prefix t

let get_decisions_list_prefix t =
  t, t.parameters.repository_for_decisions

let get_decisions_list_repository t =
  get_rep_gen get_bdd get_decisions_list_prefix t

let get_admissions_list_prefix t =
  t, t.parameters.repository_for_admissions

let get_admissions_list_repository t =
  get_rep_gen get_bdd get_admissions_list_prefix t

let get_compensations_list_prefix t =
    t, t.parameters.repository_for_compensation

let get_compensations_list_repository t =
  get_rep_gen get_bdd get_compensations_list_prefix t

let get_dispenses_list_prefix t =
  t, t.parameters.repository_for_dispenses

let get_dispenses_list_repository t =
      get_rep_gen get_bdd get_dispenses_list_prefix t

let get_additional_courses_list_prefix t =
  t, t.parameters.repository_for_additional_courses
let get_additional_courses_list_repository t =
  get_rep_gen get_bdd get_additional_courses_list_prefix t

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
  let error_log =
    Exception.empty_error_handler
  in
  let std_logger =
    Some
      (Loggers.open_logger_from_formatter Format.std_formatter)
  in
  let date = Tools.date () in
  let parameters =
    get_option_quick parameters
  in
  let rep = parameters.tmp_error_repository in
  let file = parameters.error_log_file  in
  let error_logger =
    let fic = open_out (Printf.sprintf "%s/%s" rep file) in
    Some
      (Loggers.open_logger_from_channel
         ~mode:Loggers.TXT
         fic)
  in
  let rep = parameters.tmp_profiling_repository in
  let file = parameters.profiling_log_file in
  let profiling_logger =
    let fic = open_out (Printf.sprintf "%s/%s" rep file) in
    Some
      (Loggers.open_logger_from_channel
         ~mode:Loggers.HTML
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
      date ;
    }
  in
  let state = get_option state in
  state


let copy pos mk cp t tmp_rep main_rep rep file =
  let output_rep =
    match main_rep, rep with
    | "","" -> ""
    | a,"" | "",a -> a
    | a,b -> Format.sprintf "%s/%s" a b
  in
  let t, output_rep = mk __POS__ t output_rep in
  let t =
    cp pos t
      (Format.sprintf "%s/%s" tmp_rep file)
      (Format.sprintf "%s/%s" output_rep file)
  in
  t

let store_errors_and_profiling_info mk cp t =
  let t,main_rep = get_dated_output_repository t in
  let tmp_rep = t.parameters.tmp_error_repository in
  let file = t.parameters.error_log_file  in
  let output_rep = t.parameters.error_log_repository in
  let t = copy __POS__ mk cp t tmp_rep main_rep output_rep file in
  let tmp_rep = t.parameters.tmp_profiling_repository in
  let file = t.parameters.profiling_log_file in
  let output_rep = t.parameters.profiling_log_file_repository in
  let t = copy __POS__ mk cp t tmp_rep main_rep output_rep file in
  t

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

let get_course_exceptions data = data.course_exceptions
let get_course_exceptions t = lift_get get_course_exceptions t
let set_course_exceptions course_exceptions data = {data with course_exceptions}
let set_course_exceptions course_exceptions t =
    lift_set set_course_exceptions course_exceptions t

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

let get_missing_grades data = data.missing_grades
let get_missing_grades t = lift_get get_missing_grades t
let set_missing_grades missing_grades data =
  {data with missing_grades}
let set_missing_grades missing_grades t =
  lift_set set_missing_grades missing_grades t

let get_non_accepted_grades data = data.non_accepted_grades
let get_non_accepted_grades t = lift_get get_non_accepted_grades t
let set_non_accepted_grades non_accepted_grades data =
  {data with non_accepted_grades}
let set_non_accepted_grades non_accepted_grades t =
  lift_set set_non_accepted_grades non_accepted_grades t

let get_missing_pictures data = data.missing_pictures
let get_missing_pictures t = lift_get get_missing_pictures t
let set_missing_pictures missing_pictures data =
  {data with missing_pictures}
let set_missing_pictures missing_pictures t =
  lift_set set_missing_pictures missing_pictures t

let get_gps_server_faillures data = data.gps_server_faillures
let get_gps_server_faillures t =
  lift_get get_gps_server_faillures t
let set_gps_server_faillures gps_server_faillures data =
    {data with gps_server_faillures}
let set_gps_server_faillures gps_server_faillures t =
    lift_set set_gps_server_faillures gps_server_faillures t

let get_missing_mentors data = data.missing_mentors
let get_missing_mentors t = lift_get get_missing_mentors t
let set_missing_mentors missing_mentors data =
  {data with missing_mentors}
let set_missing_mentors missing_mentors t =
  lift_set set_missing_mentors missing_mentors t

let get_missing_ects_attributions data = data.missing_ects_attributions
let get_missing_ects_attributions t = lift_get get_missing_ects_attributions t
let set_missing_ects_attributions missing_ects_attributions data =
  {data with missing_ects_attributions}
let set_missing_ects_attributions missing_ects_attributions t =
  lift_set set_missing_ects_attributions missing_ects_attributions t

let get_missing_internship_descriptions data =
  data.missing_internship_descriptions
let get_missing_internship_descriptions t = lift_get get_missing_internship_descriptions t
let set_missing_internship_descriptions missing_internship_descriptions data =
  {data with missing_internship_descriptions}
let set_missing_internship_descriptions missing_internship_descriptions t =
  lift_set set_missing_internship_descriptions missing_internship_descriptions t

let get_non_validated_internships data =
  data.non_validated_internships
let get_non_validated_internships t = lift_get get_non_validated_internships t
let set_non_validated_internships non_validated_internships data =
  {data with non_validated_internships}
let set_non_validated_internships non_validated_internships t =
    lift_set set_non_validated_internships non_validated_internships t

let get_ambiguous_internship_descriptions data =
    data.ambiguous_internship_descriptions
let get_ambiguous_internship_descriptions t = lift_get get_ambiguous_internship_descriptions t
let set_ambiguous_internship_descriptions ambiguous_internship_descriptions data =
    {data with ambiguous_internship_descriptions}
let set_ambiguous_internship_descriptions ambiguous_internship_descriptions t =
  lift_set set_ambiguous_internship_descriptions ambiguous_internship_descriptions t

let get_mentors data =
  data.mentors
let get_mentors t = lift_get get_mentors t
let set_mentors mentors data = {data with mentors}
let set_mentors mentors t = lift_set set_mentors mentors t

let get_national_diplomas data =
  data.national_diplomas
let get_national_diplomas t = lift_get get_national_diplomas t
let set_national_diplomas national_diplomas data =
      {data with national_diplomas}
let set_national_diplomas national_diplomas t =
  lift_set set_national_diplomas national_diplomas t

let get_dens data = data.dens
let get_dens t = lift_get get_dens t
let set_dens dens data = {data with dens}
let set_dens dens t = lift_set set_dens dens t


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

let get_additional_course data = data.additional_courses
let get_additional_course t =
  lift_get get_additional_course t
let set_additional_course additional_courses data =
    {data with additional_courses}
let set_additional_course additional_courses t =
    lift_set set_additional_course additional_courses t

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

let get_mentoring_list
    ?tuteur_firstname
    ?tuteur_lastname
    ?year
    t =
  t, Mentoring.get_mentoring_list ?year ?tuteur_lastname ?tuteur_firstname (get_mentoring t)

let get_mentoring ~firstname ~lastname ~year ?tuteur_gps t =
    let mentoring_opt =
      Mentoring.get_mentoring
        ~firstname ~lastname ~year (get_mentoring t)
    in
    match mentoring_opt with
    | Some a ->
      t, Some a
    | None ->
      t, tuteur_gps

let add_course_exception unify =
  add_gen
    get_course_exceptions
    set_course_exceptions
    (Course_exceptions.add_course_exception unify)

let get_course_exception ~codegps ~year t =
  let course_exception_opt =
    Course_exceptions.get_course_exception
      ~codegps ~year
      (get_course_exceptions t)
  in
  t, course_exception_opt

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
        "Pas de cursus pour %s%s en %s dans les fichiers du dÃ©partement"
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

let add_additional_course _ =
  add_gen
    get_additional_course
    set_additional_course
    (Cours_a_ajouter.add_additional_course)

let get_additional_course
    ~firstname ~lastname
    t =
  t,
  Cours_a_ajouter.get_additional_courses
    ~firstname ~lastname
    t.data.additional_courses

let add_decision unify =
  add_gen
    get_decisions
    set_decisions
    (Decisions.add_decision warn unify)

let get_decision
    ~firstname ~lastname ~year
    ~program ~dpt t =
  match
    Decisions.get_decision
      ~firstname ~lastname ~year
      ~program ~dpt
      t.data.decisions
  with
  | [] -> t, None
  | [a] -> t, Some a
  | _::_::_ ->
    warn
      __POS__
      "Several decisions for the same diploma, same year, and same students"
      Exit
      t,
    None

let get_decision_list
    ~firstname ~lastname
    ?year
    ?program ?dpt t =
  t, Decisions.get_decision
    ~firstname ~lastname
    ?year
    ?program ?dpt
    t.data.decisions

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
  let t, local = get_local_repository t in
  let t, rep = get_pictures_prefix t in
  match local, rep with
    | "", "" -> t, ""
    | a,"" | "",a -> t, a
    | a,b -> t, Format.sprintf "%s/%s" a b

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

let gen get set =
  let add state elt =
    let elts = get state in
    set (elt::elts) state
  in
  let get t = t, get t in
  add, get

let add_missing_picture, get_missing_pictures =
  gen get_missing_pictures set_missing_pictures
let add_gps_server_faillure, get_gps_server_faillures =
  gen get_gps_server_faillures set_gps_server_faillures

let add_missing_grade, get_missing_grades =
  gen get_missing_grades set_missing_grades
let add_non_accepted_grade, get_non_accepted_grades =
  gen get_non_accepted_grades set_non_accepted_grades
let add_dens, get_dens =
  gen get_dens set_dens
let add_national_diploma, get_national_diplomas =
  gen get_national_diplomas set_national_diplomas
let add_mentor, get_mentors =
  gen get_mentors set_mentors
let add_mentor state elt =
  add_mentor state elt

let add_missing_mentor, get_missing_mentors =
  gen get_missing_mentors set_missing_mentors
let add_missing_ects_attribution, get_missing_ects_attributions =
  gen get_missing_ects_attributions set_missing_ects_attributions
let add_missing_internship_description, get_missing_internship_descriptions =
  gen get_missing_internship_descriptions set_missing_internship_descriptions
let add_non_validated_internship, get_non_validated_internships =
    gen get_non_validated_internships set_non_validated_internships
let add_ambiguous_internship_description, get_ambiguous_internship_descriptions =
    gen get_ambiguous_internship_descriptions set_ambiguous_internship_descriptions

let get_ENSPSL_logo state =
  let state, local = get_local_repository state in
  if local = ""
  then
    state, "LOGOs/ENSPSL.png"
  else
    state, Format.sprintf "%s/LOGOs/ENSPSL.png" local
