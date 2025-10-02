type parameters =
  {
    safe_mode: bool;
    main_dpt: Public_data.main_dpt;
    home: string;
    homelist:string list ;
    cloud_synchronization_mode: Public_data.cloud_synchronization_mode ;
    cloud_client: Public_data.cloud_client ;
    cloud_client_options: string ;
    potential_cloud_repositories: string list ;
    potential_cloud_suffix: string list;
    cloud_support_dynamic_link : bool ;
    pdfgenerator : Public_data.pdf_generator ;
    pdfgenerator_options : string ;
    local_repository: string ;
    scholarships_repository: string ;
    diplomation_repository: string ;
    repository_for_cost_members:  string;
    repository_to_dump_transcripts: string ;
    distant_repository: string ;
    machine_to_access_gps: string ;
    url_to_access_annuaire: string ;
    port_to_access_gps: string;
    repository_to_access_gps: string;
    output_repository: string;
    database_repository: string;
    pegasus_repository: string;
    scolarity_repository: string;
    study_repository:string;
    parameters_repository:string;
    gps_backup_repository:string;
    enspsl_logo:string;
    enspsl_logo_bis:string;
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
    url_prefix_for_photos: string;
    rel_url_prefix_for_photos: string;
    correct_rel_url_prefix_for_photos: string;
    file_retriever: Public_data.file_retriever ;
    file_retriever_options: string ;
    gps_access_options: string ;
    annuaire_access_options: string ;
    file_retriever_log_repository: string ;
    file_retriever_log_file: string;
    file_retriever_annuaire_html_file: string ;
    file_retriever_n_fail: int;
    file_retriever_max_n_fail: int;
    file_retriever_skip: bool;
    tmp_annuaire_repository: string ;
    annuaire_check_certificate: bool ;
    include_pictures: bool;
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
    commissions_repository: string;
    commission: (string * Public_data.annee) option;
    dens_repository: string;
    diplomation_year: string option ;
    target: string option;
    repository_for_bourses: string;
    repository_for_tuteurs: string;
    repository_for_cours: string;
    repository_for_course_entry: string;
    repository_for_minors: string;
    repository_for_majors: string;
    repository_for_dens_candidate: string;
    repository_for_departements: string;
    repository_for_cursus: string;
    repository_for_diplomes: string;
    repository_for_cursus_exceptions: string;
    repository_for_decisions: string;
    repository_for_admissions: string;
    repository_for_compensation: string;
    repository_for_dispenses: string;
    repository_for_additional_courses: string;
    repository_for_courses_to_be_sorted: string;
    repository_for_sorted_courses: string;
    repository_for_internships_to_be_sorted: string;
    repository_for_sorted_internships: string;
    repository_for_grades_to_modify: string;
    repository_for_inscriptions: string;
    repository_for_pegasus_administrative_status: string;
    repository_for_pegasus_pedagogical_inscriptions: string;
    repository_for_pegasus_courses: string;
    repository_for_pegasus_notes: string;
    repository_for_pegasus_validations: string;
    repository_for_pegasus_stages: string;
    repository_to_dump_missing_pictures: string;
    repository_to_dump_non_accepted_grades: string;
    repository_to_dump_non_validated_internships: string;
    repository_to_dump_under_average_validated_grades: string;
    repository_to_dump_out_of_schooling_years: string;
    repository_to_dump_missing_grades: string;
    repository_to_dump_missing_mentors: string;
    repository_to_dump_missing_ects_attributions: string;
    repository_to_dump_courses_validated_twice: string;
    repository_to_dump_missing_internship_descriptions: string;
    repository_to_dump_ambiguous_internship_descriptions:string;
    repository_to_dump_national_diplomas: string;
    repository_to_dump_dens: string;
    repository_to_dump_mentors: string;
    repository_to_dump_missing_minors: string;
    repository_to_dump_missing_majors: string;
    repository_to_dump_missing_internship_translation: string;
    repository_to_dump_missing_course_entries: string;
    repository_to_dump_course_entries_report: string;
    repository_to_dump_dens_candidate: string;
    signature: string;
    log_mkdir: bool;
    bilinguage: bool;
    language: Public_data.language;
    repartition: Public_data.repartition;
    add_grades_without_registration: bool ;
    load_gps_data: bool ;
    log_pegasus_entries: bool ;
  }


let parameters =
  {
    safe_mode = true;
    main_dpt = Public_data.DI;
    log_mkdir = false;
    cloud_synchronization_mode = Public_data.CommandLine ;
    cloud_client = Public_data.NextCloudCmd ;
    cloud_client_options = "-n --silent" ;
    home = "";
    homelist =
          [
          "/users/absint3/feret/";
          "/Users/feret/"
          ];
    potential_cloud_suffix = ["Nextcloud"];
    potential_cloud_repositories =
          [] ;
    cloud_support_dynamic_link = false ;
    pdfgenerator = Public_data.PdfLatex ;
    pdfgenerator_options = "-interaction=nonstopmode";
    local_repository = "di/suivi_pedagogique" ;
    enspsl_logo = "LOGOs/ENSPSL.png" ;
    enspsl_logo_bis = "LOGOs/ENSPSL2.png" ;
    scholarships_repository = "di/scolarite/ELEVES" ;
    diplomation_repository = "di/scolarite/diplomation" ;
    repository_to_dump_transcripts = "fiches_de_notes" ;
    distant_repository = "https://cloud.di.ens.fr/" ;
    machine_to_access_gps = "violette.ens.fr" ;
    url_to_access_annuaire = "http://annuaireweb.ens.fr/" ;
    port_to_access_gps = "8080";
    repository_to_access_gps = "gps";
    output_repository = "sortie" ;
    database_repository = "base_de_donnees";
    pegasus_repository = "pegasus_data";
    scolarity_repository = "scolarite_data";
    study_repository = "etudes";
    parameters_repository = "parametres" ;
    gps_backup_repository = "gps_backup" ;
    repository_to_dump_gps_files = "fiches_de_notes";
    repository_to_dump_issues = "problemes";
    repository_to_dump_reports = "rapports";
    repository_to_dump_missing_pictures = "photos_manquantes";
    repository_to_dump_gps_server_faillures = "echecs_extraction_gps";
    repository_to_dump_non_accepted_grades = "notes_non_acceptees" ;
    repository_to_dump_under_average_validated_grades = "notes_sous_la_moyenne_validees" ;
    repository_to_dump_out_of_schooling_years = "cours_hors_scolarite" ;
    repository_to_dump_non_validated_internships = "stages_non_acceptes";
    repository_to_dump_attestations = "attestations";
    repository_to_dump_missing_grades = "notes_manquantes";
    repository_to_dump_missing_mentors = "tuteurs_manquants";
    repository_to_dump_missing_ects_attributions = "ects_non_attribuees";
    repository_to_dump_courses_validated_twice = "valides_deux_fois" ;
    repository_to_dump_missing_internship_descriptions = "stages_manquants";
    repository_to_dump_ambiguous_internship_descriptions = "stages_ambigus";
    repository_to_dump_missing_course_entries = "cours_non_traduits_par_libelle";
    repository_to_dump_course_entries_report = "cours";
    repository_for_bourses = "bourses";
    repository_for_tuteurs = "tuteurs";
    repository_for_cours = "cours";
    repository_for_cost_members = "cost";
    repository_for_course_entry = "cours_traduction";
    repository_for_departements = "departements";
    repository_for_cursus = "cursus";
    repository_for_diplomes = "diplomes";
    repository_for_cursus_exceptions = "exceptions_cursus";
    repository_for_decisions = "decisions";
    repository_for_admissions = "admissions";
    repository_for_compensation = "compensations";
    repository_for_dispenses = "dispenses";
    repository_for_additional_courses = "cours_a_ajouter";
    repository_for_courses_to_be_sorted = "cours_a_trier";
    repository_for_internships_to_be_sorted = "stages_a_trier";
    repository_for_sorted_courses = "cours_tries";
    repository_for_sorted_internships = "stages_tries";
    repository_for_grades_to_modify = "notes_a_modifier" ;
    repository_for_inscriptions = "inscriptions" ;
    repository_to_dump_dens = "dens";
    repository_to_dump_national_diplomas = "diplomes_nationaux";
    repository_to_dump_mentors = "tuteurs";
    repository_for_handmade_gps_files = "fichier_gps_fait_a_la_main";
    repository_for_backup_gps_files = "fichier_gps_de_secours";
    output_alias_repository = "courant";
    store_output_according_to_their_promotions = true;
    indicate_promotions_in_gps_file_names = true;
    indicate_promotions_in_attestation_file_names = true;
    url_prefix_for_photos = "http://annuaireweb.ens.fr/photos/";
    rel_url_prefix_for_photos = "/photos/";
    correct_rel_url_prefix_for_photos = "http://annuaireweb.ens.fr";
    repository_to_access_pictures = "photos";
    pictures_stored_according_to_promotions = true ;
    picture_file_names_mention_promotion = false;
    file_retriever  = Public_data.WGET ;
    file_retriever_options = "" ;
    gps_access_options = "" ;
    annuaire_access_options = "--no-hsts" ;
    file_retriever_log_repository = "tmp" ;
    file_retriever_log_file = "gps_access.log";
    tmp_annuaire_repository = "tmp";
    include_pictures = true;
    file_retriever_annuaire_html_file = "annuaire.html";
    annuaire_check_certificate = false ;
    file_retriever_time_out_in_seconds = Some 300;
    file_retriever_checking_period_in_seconds = 5;
    file_retriever_skip = false;
    file_retriever_n_fail = 0;
    file_retriever_max_n_fail = 5;
    tmp_profiling_repository = "tmp";
    profiling_log_file_repository = "profiling_info";
    profiling_log_file = "profiling_info.html";
    tmp_error_repository = "tmp";
    error_log_repository = "erreurs_internes";
    error_log_file = "error.txt";
    comma_symbol = ',';
    dens_repository = "diplomation";
    diplomation_year = Some "2025" ;
    repository_for_minors = "mineures" ;
    repository_for_majors = "majeures" ;
    repository_for_dens_candidate = "dens_candidates" ; repository_to_dump_missing_minors = "mineures" ;
    repository_to_dump_missing_majors = "majeures" ;
    repository_to_dump_missing_internship_translation = "stages" ;
    repository_to_dump_dens_candidate = "dens_candidates" ;
    repository_for_pegasus_administrative_status = "status_administratifs" ;
    repository_for_pegasus_pedagogical_inscriptions = "programmes_pedagogiques";
    repository_for_pegasus_notes = "notes" ;
    repository_for_pegasus_validations = "validations" ;
    repository_for_pegasus_courses = "cours";
    repository_for_pegasus_stages = "stages" ;
    current_academic_year = "2025";
    commissions_repository = "commissions_des_etudes";
    add_grades_without_registration = true  ;

    commission =  None;
    target = None ;
    signature = "feret+tampon.pdf";
    bilinguage = true ;
    language  = Public_data.French;
    repartition = Public_data.Annee_de_validation_du_cours;
    load_gps_data = false ;
    log_pegasus_entries = false;
  }

let _ = parameters.parameters_repository

let set_dma parameters =
  {
    parameters with
    main_dpt = Public_data.DMA ;
    commission = None;
    local_repository = "dma/suivi_pedagogique" ;
    scholarships_repository = "dma/scolarite/ELEVES" ;
    diplomation_repository = "dma/scolarite/diplomation" ;
    repartition = Public_data.Annee_obtention_du_diplome ;
    include_pictures = false;
    load_gps_data = false ;

  }

let set_phys parameters =
  {
    parameters with
    main_dpt = Public_data.PHYS ;
    commission = None (*Some ("23 juin 2021",  "2020")*);
    local_repository = "phys/suivi_pedagogique" ;
    scholarships_repository = "phys/scolarite/ELEVES" ;
    diplomation_repository = "phys/scolarite/diplomation" ;
    repartition = Public_data.Annee_obtention_du_diplome ;
    include_pictures = false;
    load_gps_data = true ;
  }

  let set_chimie parameters =
    {
      parameters with
      main_dpt = Public_data.CHIMIE ;
      commission = None (*Some ("23 juin 2021",  "2020")*);
      local_repository = "chimie/suivi_pedagogique" ;
      scholarships_repository = "chimie/scolarite/ELEVES" ;
      diplomation_repository = "chimie/scolarite/diplomation" ;
      repartition = Public_data.Annee_obtention_du_diplome ;
      add_grades_without_registration = true  ;
      load_gps_data = true ;
      include_pictures = true;
    }

    let set_geosciences parameters =
      {
        parameters with
        main_dpt = Public_data.GEOSCIENCES ;
        commission = None (*Some ("23 juin 2021",  "2020")*);
        local_repository = "geosciences/suivi_pedagogique" ;
        scholarships_repository = "geosciences/scolarite/ELEVES" ;
        diplomation_repository = "geosciences/scolarite/diplomation" ;
        add_grades_without_registration = true  ;
        load_gps_data = false ;

        repartition = Public_data.Annee_obtention_du_diplome ;
        include_pictures = true;
      }

      let set_ibens parameters =
        {
          parameters with
          main_dpt = Public_data.IBENS ;
          commission = None (*Some ("23 juin 2021",  "2020")*);
          local_repository = "IBENS/suivi_pedagogique" ;
          scholarships_repository = "IBENS/scolarite/ELEVES" ;
          diplomation_repository = "IBENS/scolarite/diplomation" ;
          load_gps_data = false ;
          repartition = Public_data.Annee_obtention_du_diplome ;
          include_pictures = true;
        }

type data =
  {
    students: Public_data.student_id list ;
    pg_students: Public_data.student_id list ;
    status_administratifs: Pegasus_administrative_status.t;
    pedagogical_inscriptions: Pegasus_pedagogical_registrations.t;
    pegasus_notes: Pegasus_notes.t;
    pedagogical_courses: Pegasus_courses.t ;
    pedagogical_courses_dictionnary: string Public_data.StringMap.t ;  
    stages_pegasus: Pegasus_stages.t ;
    output_alias: (string * string) option ;
    scholarships: Scholarships.t;
    mentoring: Mentoring.t;
    course_exceptions: Course_exceptions.t;
    course_entries: Course_name_translation.tentry;
    dpts: Departments.t;
    programs: Programs.t;
    cursus: Cursus.t;
    inscriptions: Inscriptions.t;
    cursus_exceptions: Cursus_exception.t;
    decisions: Decisions.t;
    admissions: Admissions.t;
    compensations: Compensations.t;
    additional_courses: Cours_a_ajouter.t;
    courses_to_be_sorted: Public_data.cours_a_trier list;
    internships_to_be_sorted: Public_data.stage_a_trier list;
    sorted_courses: Cours_a_trier.t;
    sorted_internships: Stages_a_trier.t;
    notes_a_modifier : Notes_a_modifier.t ;
    dispenses: Dispenses.t;
    missing_pictures: Public_data.student list;
    gps_server_faillures: Public_data.student list;
    non_accepted_grades: Public_data.missing_grade list;
    under_average_validated_grades: Public_data.missing_grade list;
    out_of_schooling_years: Public_data.missing_grade list;
    non_validated_internships:
      Public_data.missing_internship_description list;
    missing_grades: Public_data.missing_grade list;
    missing_ects_attributions: Public_data.missing_grade list;
    courses_validated_twice: Public_data.missing_grade list;
    missing_mentors: Public_data.missing_mentor list;
    missing_internship_descriptions: Public_data.missing_internship_description list;
    minor_suggestion: Public_data.mineure_majeure list;
    major_suggestion: Public_data.mineure_majeure list;
    minors: Minor_candidates.t;
    majors: Major_candidates.t;
    missing_internship_translations: Public_data.internship list;
    ambiguous_internship_descriptions: Public_data.missing_internship_description list;
    missing_course_entries:Public_data.course_entry list;
    course_entries_report:Course_name_translation.tentry;
    mentors: Public_data.mentor list;
    national_diplomas: Public_data.diplome_national list;
    dens: Public_data.dens list;
    dens_candidates: Dens_candidates.t;
    dens_candidates_suggestion: Public_data.dens_candidate list;
    cost_members: Public_data.cost_member list;
  }

let empty_data =
  {
    students = [];
    pg_students = []; 
    status_administratifs = Pegasus_administrative_status.empty;
    pedagogical_inscriptions = Pegasus_pedagogical_registrations.empty ;
    pedagogical_courses = Pegasus_courses.empty ;
    pedagogical_courses_dictionnary = Public_data.StringMap.empty ; 
    pegasus_notes = Pegasus_notes.empty ;
    stages_pegasus = Pegasus_stages.empty ;
    scholarships = Scholarships.empty;
    mentoring = Mentoring.empty;
    course_exceptions = Course_exceptions.empty;
    course_entries = Course_name_translation.empty_course_entry;
    dpts =  Departments.empty;
    cursus = Cursus.empty;
    inscriptions = Inscriptions.empty;
    programs = Programs.empty;
    additional_courses = Cours_a_ajouter.empty;
    courses_to_be_sorted = [];
    sorted_courses = Cours_a_trier.empty;
    internships_to_be_sorted = [];
    sorted_internships = Stages_a_trier.empty;
    notes_a_modifier = Notes_a_modifier.empty;
    output_alias = None;
    cursus_exceptions = Cursus_exception.empty;
    decisions = Decisions.empty;
    admissions = Admissions.empty;
    compensations = Compensations.empty;
    dispenses = Dispenses.empty;
    missing_pictures = [];
    gps_server_faillures = [];
    non_accepted_grades = [];
    under_average_validated_grades = [];
    out_of_schooling_years = [];
    missing_grades = [];
    missing_ects_attributions = [];
    courses_validated_twice = [];
    missing_mentors = [];
    missing_internship_descriptions = [];
    ambiguous_internship_descriptions = [];
    missing_course_entries = [];
    course_entries_report = Course_name_translation.empty_course_entry;
    non_validated_internships = [];
    mentors = [];
    national_diplomas = [];
    dens = [];
    minor_suggestion = [];
    major_suggestion = [];
    missing_internship_translations = [];
    dens_candidates = Dens_candidates.empty;
    minors = Minor_candidates.empty;
    majors = Major_candidates.empty;
    dens_candidates_suggestion = [] ;
    cost_members = [];
  }

type t =
  {
    parameters : parameters ;
    cloud_repository: string option ;
    prefix : string ;
    error_log : Exception.method_handler ;
    profiling_info : Profiling.log_info option ;
    std_logger : Loggers.t option ;
    profiling_logger: Loggers.t option ;
    error_logger: Loggers.t option ;
    data: data;
    date: string;
    copy_stack:(string*string*string) list;
  }

type pos = string*int*int*int

type 'a unify = (string * int * int * int -> t -> 'a -> 'a -> t * 'a)
type 'a add = 'a unify -> pos -> 'a -> t -> t


let log_mkdir t = t,t.parameters.log_mkdir

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

  let get_error_handler t =
    t, t.error_log

  let set_error_handler error_log t =
    {t with error_log}

  let get_std_logger t =
      get_logger_gen (fun x -> x.std_logger) t

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


let get_cloud_client t =
  t, t.parameters.cloud_client

let check_potential_cloud_repository h =
  try
    Sys.is_directory h
  with
    Sys_error _ -> false


let set_cloud_repository t =
  let rec aux l =
    match l with
    | [] -> failwith "Cannot find the cloud repository"
    | head::tail ->
      if check_potential_cloud_repository head
      then
        (Format.printf "%s" head;
        {t with cloud_repository = Some head}, head)
      else
        aux tail
  in
  aux (t.parameters.potential_cloud_repositories)

let get_cloud_repository t =
  match
    t.cloud_repository
  with
  | Some c -> t,c
  | None -> set_cloud_repository t

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

let get_all_potential_cloud_repositories t =
  t, t.parameters.potential_cloud_repositories

let get_all_potential_local_repositories t =
  let t,l = get_all_potential_cloud_repositories t in
  let local = t.parameters.local_repository in
  t,
  List.rev_map
    (fun s -> Printf.sprintf "%s/%s" s local)
    (List.rev l)

let get_distant_repository t =
  t,t.parameters.distant_repository

let get_cloud_client_options t =
  t,t.parameters.cloud_client_options

let get_url_prefix_for_photos t =
  t, t.parameters.url_prefix_for_photos

let get_rel_url_prefix_for_photos t =
  t, t.parameters.rel_url_prefix_for_photos

let get_correct_rel_url_prefix_for_photos t =
  t, t.parameters.correct_rel_url_prefix_for_photos

let get_include_pictures t =
  t, t.parameters.include_pictures

let get_file_retriever t =
  t, t.parameters.file_retriever

let get_file_retriever_options t =
  t,
  t.parameters.file_retriever_options

let get_annuaire_access_options t =
  let certificate =
    if t.parameters.annuaire_check_certificate then
      ""
    else
      " --no-check-certificate"
  in
  t,
  Printf.sprintf "%s%s"
    t.parameters.annuaire_access_options
    certificate

let get_gps_access_options t =
  t, t.parameters.gps_access_options

let get_file_retriever_options ?more_options t =
  let t, option1 = get_file_retriever_options t in
  let t, option2 =
    match more_options with
    | None -> t, ""
    | Some get -> get t
  in
  match option1,option2 with
  | "",a | a,"" -> t,a
  | a,b -> t,Format.sprintf "%s %s" a b

let get_file_retriever_log_repository t =
  t,
  t.parameters.home^t.parameters.file_retriever_log_repository

let get_file_retriever_log_file t =
  t,
  t.parameters.file_retriever_log_file

let get_file_retriever_annuaire_tmp_repository t =
  t,
  t.parameters.home^t.parameters.tmp_annuaire_repository

let get_file_retriever_annuaire_html_file t =
  t,
  t.parameters.file_retriever_annuaire_html_file

let get_file_retriever_time_out_in_second t =
  t, t.parameters.file_retriever_time_out_in_seconds

let get_file_retriever_checking_period t =
  t, t.parameters.file_retriever_checking_period_in_seconds

let get_machine_to_access_gps t =
  t, t.parameters.machine_to_access_gps

let get_port_to_access_gps t =
  t, t.parameters.port_to_access_gps

let get_url_to_access_annuaire t =
  t, t.parameters.url_to_access_annuaire

let get_signature t =
  let t, l = get_all_potential_cloud_repositories t in
  t,
  List.rev_map
    (fun rep ->
       if rep = ""
       then
         t.parameters.signature
       else
         Format.sprintf "%s/%s" rep t.parameters.signature
    )
    (List.rev l)

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

  let get_bdd t =
      let t, local = get_local_repository t in
      match local, t.parameters.database_repository with
      | "", "" -> t, ""
      | a,"" | "",a -> t, a
      | a,b -> t, Format.sprintf "%s/%s" a b


  let get_repository_bdd_gen get t =
    let t, rep = get_bdd t in
    match rep with
    | "" -> t, get t
    | _ -> t, Format.sprintf "%s/%s" rep (get t)

  let get_repository_to_dump_reports_gen' gen  ?output_repository t =
    let t, output =
      match output_repository with
      | None -> get_dated_output_repository t
      | Some a -> t, a
    in
    match
      output, gen
    with
    | "","" -> t, ""
    | "",a | a,""-> t, a
    | a,b -> t, Format.sprintf "%s/%s" a b


let get_repository_to_dump_reports ?output_repository t =
    get_repository_to_dump_reports_gen'
          t.parameters.repository_to_dump_reports ?output_repository t


let get_repository_to_dump_reports_gen ?output_repository gen t =
    let t, rep = get_repository_to_dump_reports ?output_repository t in
    match rep with
    | "" -> t, (gen t)
    | _ -> t, Format.sprintf "%s/%s" rep (gen t)

let get_data t = t.data
let set_data data t = {t with data}
let lift_get get t = get (get_data t)
let lift_set set data t =
    set_data (set data (get_data t)) t

module type Interface_collector_without_unification =
  sig
    type entry
    val prefix: (t -> string)->t->t * string
    val repository: (t->string)
    val get: data -> entry list
    val set: entry list -> data -> data
  end

type 'a unification = (string * int * int * int) -> t -> 'a -> 'a -> t * 'a

module type Interface_collector_with_unification =
  sig
    type entry
    type collector
    val prefix: (t -> string)->t->t * string
    val repository: (t->string)
    val get: data -> collector
    val set: collector -> data -> data
    val add: entry unification -> (string * int * int * int) -> t -> entry ->  collector -> t * collector
  end

module type Interface_collector_with_search_by_students =
    sig
      include Interface_collector_with_unification
      val find_list: firstname:string -> lastname:string -> year:string ->
        collector -> entry list
    end

module type Interface_collector_with_search_by_students_wo_year =
    sig
      include Interface_collector_with_unification
      val find_list: firstname:string -> lastname:string ->
            collector -> entry list
    end

module type Interface_translation =
   sig
      type entry
      val build: ?fr:string -> ?en:string -> string -> entry
      module Collector: Interface_collector_with_unification
             with type entry = entry
      val find_opt: string -> Collector.collector -> entry option
      val get_french: entry -> string option
      val get_english: entry -> string option

      module Report: Interface_collector_with_unification
             with type entry = entry

      module Missing: Interface_collector_without_unification
             with type entry = entry

      val report_to_list: Report.collector -> entry list
      (*val label: entry -> string*)
  end

module type Collector =
  sig
    type entry
    type collector
    val get_repository: t -> t * string
    val get: t -> t * collector
    val add: t -> entry -> t
  end

module type Collector_with_unification =
  sig
    type entry
    type collector
    val get_repository: t -> t * string
    val get: t -> t * collector
    val add: entry unification ->
             (string * int * int * int) ->
             entry -> t -> t
  end

  module type Collector_with_search_by_students =
      sig
        type entry
        type collector
        val get_repository: t -> t * string
        val get: t -> t * collector
        val add: entry unification ->
                 (string * int * int * int) ->
                 entry -> t -> t
        val find_opt: firstname:string ->     lastname:string -> year:string ->
          t -> t * entry option
        val find_list: firstname:string ->     lastname:string -> year:string ->
            t -> t * entry list
      end

      module type Collector_with_search_by_students_wo_year =
          sig
            type entry
            type collector
            val get_repository: t -> t * string
            val get: t -> t * collector
            val add: entry unification ->
                     (string * int * int * int) ->
                     entry -> t -> t
            val find_opt: firstname:string ->     lastname:string  ->
              t -> t * entry option
            val find_list: firstname:string ->     lastname:string ->
                t -> t * entry list
          end

module type Translations =
    sig
      module Collector: Collector_with_unification
      module Report: Collector_with_unification
      module Missing: Collector with type collector = Collector.entry list

      val get_translation: Collector.entry unification -> (string * int * int * int) -> string -> t -> t * (string option * string option)
      val get_report: t -> t * Collector.entry list
    end


(* Warnings *)
module Make_list_collector(I:Interface_collector_without_unification) =
  (struct
    type entry = I.entry
    type collector = I.entry list

    let get_repository = I.prefix I.repository
    let add a b = a::b
    let get t = t,lift_get I.get t
    let store a b = lift_set I.set b a
    let add t a  =
      let t, collector = get t in
      let collector = add a collector in
      store t collector
  end: Collector with type entry = I.entry
      and type collector = I.entry list )


module Make_collector_with_unification(I:Interface_collector_with_unification) =
  (struct
    type entry = I.entry
    type collector = I.collector

    let get_repository = I.prefix I.repository
    (*let add a b = I.add a b*)
    let get t = t,lift_get I.get t
    let store a b = lift_set I.set b a
    let add pos unify entry t  =
      let t, collector = get t in
      let t, collector  = I.add pos unify t entry collector in
      store t collector
  end: Collector_with_unification with type entry = I.entry
      and type collector = I.collector )

  module Make_collector_with_search_by_students(I:Interface_collector_with_search_by_students) =
        (struct
          include Make_collector_with_unification(I)

          let find_list ~firstname ~lastname ~year t =
              let t, collector = get t in
              t, I.find_list ~firstname ~lastname ~year collector

          let find_opt ~firstname ~lastname ~year t =
            match
             find_list ~firstname ~lastname ~year t
            with
            | t, [] -> t, None
            | t, [a] -> t, Some a
            | t, _::_::_ ->
                      warn
                        __POS__
                        "Several dens candidates for the same year and the same student"
                        Exit
                        t,
                      None
        end: Collector_with_search_by_students with type entry = I.entry
            and type collector = I.collector )


module Make_collector_with_search_by_students_wo_year(I:Interface_collector_with_search_by_students_wo_year) =
  (struct
    include Make_collector_with_unification(I)

    let find_list ~firstname ~lastname t =
        let t, collector = get t in
        t, I.find_list ~firstname ~lastname collector

    let find_opt ~firstname ~lastname  t =
        match
          find_list ~firstname ~lastname t
        with
        | t, [] -> t, None
        | t, [a] -> t, Some a
        | t, _::_::_ ->
          warn
            __POS__
            "Several dens candidates for the same year and the same student"
            Exit
            t,
            None
  end: Collector_with_search_by_students_wo_year with type entry = I.entry
                      and type collector = I.collector )

let simplify s =
    Special_char.lowercase (Special_char.correct_string_txt (String.trim s))

module Make_collector_translation(I:Interface_translation)  =
          (struct
            module Collector = Make_collector_with_unification(I.Collector)
            (*include Collector*)
            module Report = (Make_collector_with_unification(I.Report): Collector_with_unification with type entry = I.Report.entry and
            type collector = I.Report.collector)
            module Missing = (Make_list_collector(I.Missing): Collector with type entry = I.Missing.entry and type collector = I.Missing.entry list)


            let find_opt string t =
              let t, collector = Collector.get t in
              t, I.find_opt string collector

          let not_empty string =
            not (string = "" || (String.sub string 0 1 = "\"" &&
             String.trim (String.sub string 1 ((String.length string)-1))
             = "\""))

          let get_translation unify pos label t =
            let label = simplify label in
            let t, a_opt = find_opt label t in
            let l_fr_opt, l_en_opt =
              match
                a_opt
              with
                | None -> None,None
                | Some a -> I.get_french a, I.get_english a
            in
            let t, l_fr_opt, l_en_opt =
              match l_fr_opt, l_en_opt, a_opt with
                | Some x, Some y, Some a  when not_empty x && not_empty y ->
                       Report.add unify pos a t, l_fr_opt, l_en_opt
                | _ ->
                begin
                  let l = String.split_on_char '&' label in
                  match l with
                      | [l_fr;l_en] ->
                        let fr = Some l_fr in
                        let en = Some l_en in
                        Missing.add t (I.build ?fr ?en label),
                        fr, en
                      | _ -> Missing.add t (I.build label), None, None
                end
            in
            let l_fr_opt =
                match l_fr_opt
                with None -> Some label
                  | _ -> l_fr_opt
            in
            let l_en_opt =
                match l_en_opt
                with None -> l_fr_opt | _ -> l_en_opt
            in
            t, (l_fr_opt, l_en_opt)

           let get_report t =
              let t, collector = Report.get t in
              t, I.report_to_list collector
          end: Translations
            with type Collector.entry = I.entry
             and type Report.entry = I.entry
             and type Missing.entry = I.entry
             and type Missing.collector = I.entry list )


  let add_gen get set add pos data t =
    let t, acc =
        add pos t data (get t)
    in
    set acc t

let add_gen_list get set _unify  =
   add_gen get set (fun _ t a b -> t, a::b)

let add_gen_unify get set add unify =
   add_gen get set (add unify)

let add_gen_unify_warn get set add unify =
  add_gen get set (add warn unify)

(* Warnings about the pictures that are missing *)
module Missing_pictures =
    Make_list_collector
        (struct
          type entry = Public_data.student
          let prefix = get_repository_to_dump_missing_gen
          let repository t = t.parameters.repository_to_dump_missing_pictures
          let get data = data.missing_pictures
          let set missing_pictures data = {data with missing_pictures}
        end: Interface_collector_without_unification with type entry = Public_data.student)

module Student_ids =
   Make_list_collector
      (struct
        type entry = Public_data.student_id
        let prefix t = get_repository_bdd_gen t
        let repository _t = "etudiants"
        let get data = data.students
        let set students data = {data with students}

      end: Interface_collector_without_unification with type entry = Public_data.student_id)

module Pg_students =
    Make_list_collector
         (struct
           type entry = Public_data.student_id
           let prefix t = get_repository_bdd_gen t
           let repository _t = "pg"
           let get data = data.pg_students
           let set pg_students data = {data with pg_students}
   
         end: Interface_collector_without_unification with type entry = Public_data.student_id)

(* Warnings about failure in gps accesses *)
module Gps_server_faillures =
  Make_list_collector
      (struct
        type entry = Public_data.student
        let prefix = get_repository_to_dump_missing_gen
        let repository t = t.parameters.repository_to_dump_gps_server_faillures
        let get data = data.gps_server_faillures
        let set gps_server_faillures data = {data with gps_server_faillures}
      end: Interface_collector_without_unification with type entry = Public_data.student )

(* Warning about internships *)
module Ambiguous_internship_descriptions =
  Make_list_collector
      (struct
          type entry = Public_data.missing_internship_description
          let prefix = get_repository_to_dump_missing_gen
          let repository t = t.parameters.repository_to_dump_ambiguous_internship_descriptions
          let get data = data.ambiguous_internship_descriptions
          let set ambiguous_internship_descriptions data = {data with ambiguous_internship_descriptions}
        end: Interface_collector_without_unification
              with type entry = Public_data.missing_internship_description)


(* Warning about internships *)
module Non_validated_internships =
  Make_list_collector
    (struct
        type entry = Public_data.missing_internship_description
        let prefix = get_repository_to_dump_missing_gen
        let repository t = t.parameters.repository_to_dump_non_validated_internships
        let get data = data.non_validated_internships
        let set non_validated_internships data = {data with non_validated_internships}
     end: Interface_collector_without_unification
                    with type entry = Public_data.missing_internship_description)

module Internships_to_be_sorted =
  Make_list_collector
      (struct
          type entry = Public_data.stage_a_trier
          let prefix = get_repository_to_dump_missing_gen
          let repository t = t.parameters.repository_for_internships_to_be_sorted
          let get data = data.internships_to_be_sorted
          let set internships_to_be_sorted data =
              {data with internships_to_be_sorted}
        end: Interface_collector_without_unification
              with type entry = Public_data.stage_a_trier)

module Missing_internship_translations =
  Make_list_collector
    (struct
        type entry = Public_data.internship
        let prefix = get_repository_to_dump_missing_gen
        let repository t = t.parameters.repository_to_dump_missing_internship_translation
        let get data = data.missing_internship_translations
        let set missing_internship_translations data = {data with missing_internship_translations}
     end: Interface_collector_without_unification
            with type entry = Public_data.internship)

module Missing_internship_descriptions =
        Make_list_collector
            (struct
                type entry = Public_data.missing_internship_description
                let prefix = get_repository_to_dump_missing_gen
                let repository t = t.parameters.repository_to_dump_missing_internship_descriptions
                let get data = data.missing_internship_descriptions
                let set missing_internship_descriptions data = {data with missing_internship_descriptions}
              end: Interface_collector_without_unification
                  with type entry = Public_data.missing_internship_description)

(** Warnings about grades *)
module Missing_grades =
  Make_list_collector
      (struct
          type entry = Public_data.missing_grade
          let prefix = get_repository_to_dump_missing_gen
          let repository t = t.parameters.repository_to_dump_missing_grades
          let get data = data.missing_grades
          let set missing_grades data = {data with missing_grades}
        end: Interface_collector_without_unification
              with type entry = Public_data.missing_grade)

module Non_accepted_grades =
  Make_list_collector
    (struct
        type entry = Public_data.missing_grade
        let prefix = get_repository_to_dump_missing_gen
        let repository t = t.parameters.repository_to_dump_non_accepted_grades
        let get data = data.non_accepted_grades
        let set non_accepted_grades data = {data with non_accepted_grades}
    end: Interface_collector_without_unification with type entry = Public_data.missing_grade)

module Under_average_validated_grades =
  Make_list_collector
    (struct
      type entry = Public_data.missing_grade
      let prefix = get_repository_to_dump_missing_gen
      let repository t = t.parameters.repository_to_dump_under_average_validated_grades
      let get data = data.under_average_validated_grades
      let set under_average_validated_grades data = {data with under_average_validated_grades}
    end: Interface_collector_without_unification with type entry = Public_data.missing_grade)

module Missing_ects_attributions =
  Make_list_collector
    (struct
        type entry = Public_data.missing_grade
        let prefix = get_repository_to_dump_missing_gen
        let repository t = t.parameters.repository_to_dump_missing_ects_attributions
        let get data = data.missing_ects_attributions
        let set missing_ects_attributions data = {data with missing_ects_attributions}
    end: Interface_collector_without_unification with type entry = Public_data.missing_grade)

module Courses_validated_twice =
  Make_list_collector
    (struct
        type entry = Public_data.missing_grade
        let prefix = get_repository_to_dump_missing_gen
        let repository t = t.parameters.repository_to_dump_courses_validated_twice
        let get data = data.courses_validated_twice
        let set courses_validated_twice data = {data with courses_validated_twice}
    end: Interface_collector_without_unification with type entry = Public_data.missing_grade)

(** Warning about DENS *)
module Dens_candidate_suggestion =
  Make_list_collector
    (struct
      type entry = Public_data.dens_candidate
      let prefix = get_repository_to_dump_missing_gen
      let repository t = t.parameters.repository_to_dump_dens_candidate
      let get data = data.dens_candidates_suggestion
      let set dens_candidates_suggestion data = {data with dens_candidates_suggestion}
     end: Interface_collector_without_unification with type entry = Public_data.dens_candidate)

module Collector_dens_candidate =
  Make_collector_with_search_by_students
    (struct
      type entry = Public_data.dens_candidate
      type collector = Dens_candidates.t

      let prefix = get_repository_bdd_gen
      let repository t = t.parameters.repository_for_dens_candidate

      let get data = data.dens_candidates
      let set dens_candidates data = {data with dens_candidates}
      let add = Dens_candidates.add_dens_candidate
      let find_list = Dens_candidates.get_dens_candidate
    end)

module Collector_minor_candidate =
  Make_collector_with_search_by_students
      (struct
        type entry = Public_data.mineure_majeure
          type collector = Minor_candidates.t

          let prefix = get_repository_bdd_gen
          let repository t = t.parameters.repository_for_minors

          let get data = data.minors
          let set minors data = {data with minors}
          let add = Minor_candidates.add_minor_candidate
          let find_list ~firstname ~lastname ~year c = Minor_candidates.get_minor_candidate ~firstname ~lastname ~year c
        end)

module Collector_major_candidate =
  Make_collector_with_search_by_students
      (struct
        type entry = Public_data.mineure_majeure
        type collector = Major_candidates.t

        let prefix = get_repository_bdd_gen
        let repository t = t.parameters.repository_for_majors

        let get data = data.majors
        let set majors data = {data with majors}
        let add = Major_candidates.add_major_candidate
        let find_list ~firstname ~lastname ~year c = Major_candidates.get_major_candidate ~firstname ~lastname ~year c
      end)

module Dens_candidate_missing_minors =
  Make_list_collector
    (struct
      type entry = Public_data.mineure_majeure
      let prefix = get_repository_to_dump_missing_gen
      let repository t = t.parameters.repository_to_dump_missing_minors
      let get data = data.minor_suggestion
      let set minor_suggestion data = {data with minor_suggestion}
    end: Interface_collector_without_unification with type entry = Public_data.mineure_majeure)

module Dens_candidate_missing_majors =
  Make_list_collector
    (struct
      type entry = Public_data.mineure_majeure
      let prefix = get_repository_to_dump_missing_gen
      let repository t = t.parameters.repository_to_dump_missing_majors
      let get data = data.major_suggestion
      let set major_suggestion data = {data with major_suggestion}
     end: Interface_collector_without_unification with type entry = Public_data.mineure_majeure)

module Course_to_be_sorted =
  Make_list_collector
    (struct
      type entry = Public_data.cours_a_trier
      let prefix = get_repository_to_dump_missing_gen
      let repository t = t.parameters.repository_for_courses_to_be_sorted
      let get data = data.courses_to_be_sorted
      let set courses_to_be_sorted data = {data with courses_to_be_sorted}
    end: Interface_collector_without_unification with type entry = Public_data.cours_a_trier)

(** Other warnings *)
module Grade_out_of_schooling_years =
  Make_list_collector
    (struct
      type entry = Public_data.missing_grade
      let prefix t = get_repository_to_dump_missing_gen t
      let repository t = t.parameters.repository_to_dump_out_of_schooling_years
      let get data = data.out_of_schooling_years
      let set out_of_schooling_years data = {data with out_of_schooling_years}
     end: Interface_collector_without_unification with type entry = Public_data.missing_grade)

module Missing_mentors =
  Make_list_collector
    (struct
      type entry = Public_data.missing_mentor
      let prefix = get_repository_to_dump_missing_gen
      let repository t = t.parameters.repository_to_dump_missing_mentors
      let get data = data.missing_mentors
      let set missing_mentors data = {data with missing_mentors}
     end: Interface_collector_without_unification with type entry = Public_data.missing_mentor)

module Collector_dens_diplomas =
  Make_list_collector
  (struct
    type entry = Public_data.dens
    let prefix t = get_repository_to_dump_reports_gen t
    let repository t = t.parameters.repository_to_dump_dens
    let get data = data.dens
    let set dens data = {data with dens}
   end: Interface_collector_without_unification with type entry = Public_data.dens)

module Collector_national_diplomas =
     Make_list_collector
     (struct
       type entry = Public_data.diplome_national
       let prefix t = get_repository_to_dump_reports_gen t
       let repository t = t.parameters.repository_to_dump_national_diplomas
       let get data = data.national_diplomas
       let set national_diplomas data = {data with national_diplomas}
      end: Interface_collector_without_unification with type entry = Public_data.diplome_national)

module Collector_mentors =
    Make_list_collector
      (struct
        type entry = Public_data.mentor
        let prefix t = get_repository_to_dump_reports_gen t
        let repository t = t.parameters.repository_to_dump_mentors
        let get data = data.mentors
        let set mentors data = {data with mentors}
      end: Interface_collector_without_unification with type entry = Public_data.mentor)

(* reports *)
(*let get_repository_to_dump_course_entries_report,
    get_course_entries_report,
    _set_course_entries_report,
    add_course_entry_in_report =
  gen_report_report
    {report_prefix = (fun t -> get_repository_to_dump_reports_gen t);
     report_rep = (fun t ->
       t.parameters.repository_to_dump_course_entries_report);
    report_get = (fun data -> data.course_entries_report) ;
    report_set = (fun course_entries_report data -> {data with course_entries_report} );
    report_add = Course_name_translation.add_course_entry;

    report_get_entry = (fun entry -> entry.Public_data.gps_entry);
    report_set_entry = (fun entry gps_entry -> {entry with Public_data.gps_entry}) ;

    }*)

(** gps files *)
let get_repository_to_dump_gps_files ?output_repository t =
      get_repository_to_dump_reports_gen' t.parameters.repository_to_dump_gps_files
      ?output_repository t

(* SAD *)
let get_repository_to_dump_dens_supplement ?output_repository t =
    get_repository_to_dump_reports_gen
      (fun t -> t.parameters.dens_repository)
      ?output_repository t

(* attestations *)
let get_repository_to_dump_attestations t =
    get_repository_to_dump_reports_gen
      (fun t -> t.parameters.repository_to_dump_attestations)
      t

(* Backup repositories *)
let get_backup_repository t =
  let t, local = get_local_repository t in
  match local, t.parameters.gps_backup_repository with
  | "", "" -> t, ""
  | a,"" | "",a -> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_backup_gen gen_rep t =
  let t, rep =
    get_backup_repository t
  in
  match
    rep,
    gen_rep
  with
  | "","" -> t,""
  | "",a | a,"" -> t,a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_repository_for_handmade_gps_files t =
    get_backup_gen t.parameters.repository_for_handmade_gps_files t

let get_repository_for_backup_gps_files t =
  get_backup_gen t.parameters.repository_for_backup_gps_files t

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

  let get_course_entry_list_prefix t =
                t, t.parameters.repository_for_course_entry


let get_scolarity_bdd t =
  let t, cloud = get_cloud_repository t in
  match cloud, t.parameters.scolarity_repository with
    | "", "" -> t, ""
    | a,"" | "",a -> t, a
    | a,b -> t, Format.sprintf "%s/%s" a b

(*let get_scolarity_bdd_gen get t =
  let t, rep = get_scolarity_bdd t in
  match rep with
    | "" -> t, get t
    | _ -> t, Format.sprintf "%s/%s" rep (get t)*)

  let get_course_entry_list_repository t =
      get_rep_gen get_scolarity_bdd get_course_entry_list_prefix t

module Collector_stages_tries =
  Make_collector_with_unification
    (struct
      type entry = Public_data.stage_a_trier
      type collector = Stages_a_trier.t

      let prefix = get_repository_bdd_gen
      let repository t =  t.parameters.repository_for_sorted_internships
      let get data =  data.sorted_internships
      let set sorted_internships data = {data with sorted_internships}
      let add = Stages_a_trier.add_sorted_internship
    end: Interface_collector_with_unification
    with type entry = Public_data.stage_a_trier
    and type collector = Stages_a_trier.t )

let get_study t =
  let t, local = get_local_repository t in
  match local, t.parameters.study_repository with
  | "", "" -> t, ""
  | a,"" | "",a -> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_study_gen get t =
  let t, rep = get_study t in
  match rep with
  | "" -> t, get t
  | _ -> t, Format.sprintf "%s/%s" rep (get t)

let get_pegasus t =
  let t, cloud = get_cloud_repository t in
  match cloud, t.parameters.pegasus_repository with
    | "", "" -> t, ""
    | a,"" | "",a -> t, a
    | a,b -> t, Format.sprintf "%s/%s" a b

let get_pegasus_gen get t =
  let t, rep = get_pegasus t in
  match rep with
  | "" -> t, get t
  | _ -> t, Format.sprintf "%s/%s" rep (get t)


  module Collector_pegasus_stages =
    Make_collector_with_search_by_students_wo_year
      (struct
        type entry = Public_data.stage_pegasus
        type collector = Pegasus_stages.t

        let prefix = get_repository_bdd_gen
        let repository t =  t.parameters.repository_for_pegasus_stages
        let get data =  data.stages_pegasus
        let set stages_pegasus data = {data with stages_pegasus}
        let add = Pegasus_stages.add_pegasus_stage
        let find_list = Pegasus_stages.get_pegasus_stages

    end: Interface_collector_with_search_by_students_wo_year
    with type entry = Public_data.stage_pegasus
    and type collector = Pegasus_stages.t )

  module Collector_pedagogical_registrations  =
    Make_collector_with_search_by_students_wo_year
      (struct
        type entry = Public_data.pedagogical_entry_pegasus list
        type collector = Pegasus_pedagogical_registrations.t

        let prefix = get_pegasus_gen
        let repository t = t.parameters.repository_for_pegasus_pedagogical_inscriptions
        let get data =  data.pedagogical_inscriptions
        let set pedagogical_inscriptions data = {data with pedagogical_inscriptions}
        let add = Pegasus_pedagogical_registrations.add_pegasus_pedagocial_registrations
        let find_list = Pegasus_pedagogical_registrations.get_pegasus_pedagocial_registrations

      end: Interface_collector_with_search_by_students_wo_year
      with type entry = Public_data.pedagogical_entry_pegasus list
      and type collector = Pegasus_pedagogical_registrations.t )

  module Collector_pegasus_notes  =
        Make_collector_with_unification
        (struct

            type entry = Public_data.note_pegasus
            type collector = Pegasus_notes.t

            let prefix = get_pegasus_gen
            let repository t = t.parameters.repository_for_pegasus_notes
            let get data =  data.pegasus_notes
            let set pegasus_notes data = {data with pegasus_notes}
            let add = Pegasus_notes.add_pegasus_note
          end: Interface_collector_with_unification
          with type entry = Public_data.note_pegasus
          and type collector = Pegasus_notes.t )

module Collector_pegasus_validations  =
                Make_collector_with_unification
                  (struct
                    type entry = Public_data.note_pegasus
                    type collector = Pegasus_notes.t

                    let prefix = get_pegasus_gen
                    let repository t = t.parameters.repository_for_pegasus_validations
                    let get data =  data.pegasus_notes
                    let set pegasus_notes data = {data with pegasus_notes}
                    let add = Pegasus_notes.add_pegasus_note

                  end: Interface_collector_with_unification
                  with type entry = Public_data.note_pegasus
                  and type collector = Pegasus_notes.t )

module Collector_course_pegasus =
    Make_collector_with_unification
      (struct
          type entry = Public_data.course_pegasus
          type collector = Pegasus_courses.t

          let prefix = get_pegasus_gen
          let repository t = t.parameters.repository_for_pegasus_courses
          let get data = data.pedagogical_courses
          let set pedagogical_courses data = {data with pedagogical_courses}
          let add unify pos state course courses =
            let state, course =
              match course.Public_data.pegasus_session, course.Public_data.pegasus_de_a with 
                | None, None ->   
                  warn __POS__ (Format.sprintf "Either a date or a session number is required")
                        Exit state, course  
                | _, Some pegasus_de_a -> 
                  if String.length pegasus_de_a >= 10 
                  then 
                    let pegasus_year = String.sub pegasus_de_a 6 4 in 
                    let year_short = String.sub pegasus_de_a 8 2 in
                    let pegasus_session= 
                        Some (pegasus_year^course.Public_data.pegasus_helisa^"          "^year_short^"010001")
                    in 
                    state, {course with Public_data.pegasus_session ; Public_data.pegasus_year}  
                  else
                    warn
                      __POS__ (Format.sprintf "Bad range for the date of the course %s" pegasus_de_a)
                      Exit state, course        
                | Some session, _ -> 
                   let session = Tools.remove_end ~suffix:"-GENERAL" session in 
                   let course = {course with Public_data.pegasus_session = Some session} in 
                  if String.length session > 3 then
                    let pegasus_year = String.sub session 0 4 in 
                    state, {course with Public_data.pegasus_year } 
                  else
                    warn
                      __POS__ (Format.sprintf "Bad session number %s" session)
                      Exit state, course
            in
            let course =
              match  course.Public_data.pegasus_codegps with
              | None | Some "" -> {course with Public_data.pegasus_codegps = Some course.Public_data.pegasus_helisa}
              | Some x ->
                if String.length x > 1 && String.sub x 0 2 = "XT"
                then {course with Public_data.pegasus_codegps = Some course.Public_data.pegasus_helisa}
                else
                  course
            in
            Pegasus_courses.add_pegasus_course unify pos state course courses

      end: Interface_collector_with_unification
      with type entry = Public_data.course_pegasus
        and type collector = Pegasus_courses.t)

let set_pedagogical_courses_dictionnary map state = 
  let data = {state.data with pedagogical_courses_dictionnary = map} in 
  {state with data} 

let get_pedagogical_courses_dictionnary state = 
  state, state.data.pedagogical_courses_dictionnary
  
module Collector_administrative_status =
  Make_collector_with_search_by_students
    (struct
      type entry = Public_data.student_pegasus
      type collector = Pegasus_administrative_status.t

      let prefix = get_pegasus_gen
      let repository t = t.parameters.repository_for_pegasus_administrative_status
      let get data =  data.status_administratifs
      let set status_administratifs data = {data with status_administratifs}
      let add = Pegasus_administrative_status.add_pegasus_administrative_status
      let find_list = Pegasus_administrative_status.get_pegasus_administrative_status

    end: Interface_collector_with_search_by_students
    with type entry = Public_data.student_pegasus
    and type collector = Pegasus_administrative_status.t )

module Collector_scholarships =
  Make_collector_with_search_by_students_wo_year
    (struct
      type entry = Public_data.scholarship
      type collector = Scholarships.t

      let prefix = get_repository_bdd_gen
      let repository t = t.parameters.repository_for_bourses
      let get data =  data.scholarships
      let set scholarships data = {data with scholarships}
      let add = Scholarships.add_scholarship
      let find_list = Scholarships.get_scholarship

    end: Interface_collector_with_search_by_students_wo_year
    with type entry = Public_data.scholarship
    and type collector = Scholarships.t )

module Collector_course_exceptions =
  Make_collector_with_unification
    (struct
      type entry = Public_data.course_exception
      type collector = Course_exceptions.t

      let prefix = get_repository_bdd_gen
      let repository t = t.parameters.repository_for_cours
      let get data =  data.course_exceptions
      let set course_exceptions data = {data with course_exceptions}
      let add = Course_exceptions.add_course_exception

    end: Interface_collector_with_unification
    with type entry = Public_data.course_exception
    and type collector = Course_exceptions.t )

    let get_course_in_pegasus ~codehelisa ~year t =
        let code = Special_char.lowercase codehelisa in
        let t, collector = Collector_course_pegasus.get t in
        let course_opt =
               Pegasus_courses.get_pegasus_course
                  ~code ~year collector
        in
        t, course_opt

    let get_course_in_pegasus_by_libelle ~libelle ~year ~semester t =
        let libelle = Special_char.lowercase libelle in
        let t, collector = Collector_course_pegasus.get t in
        let course_opt =
              Pegasus_courses.get_pegasus_course_by_libelle
                 ~libelle ~year ~semester collector
        in
        match course_opt with 
        | _::_ -> t, course_opt
        | [] -> 
          let libelle = Tools.hash_libelle libelle in 
          match Public_data.StringMap.find_opt libelle t.data.pedagogical_courses_dictionnary with 
          | None -> t, []
          | Some libelle -> 
            let libelle = Special_char.lowercase libelle in
            let course_opt =
                  Pegasus_courses.get_pegasus_course_by_libelle
                     ~libelle ~year ~semester collector
            in
            t, course_opt 

let get_grade_in_pegasus ~codehelisa ~year ~firstname ~lastname t =
    let code = Special_char.lowercase codehelisa in
    let firstname = Special_char.lowercase firstname in
    let lastname = Special_char.lowercase lastname in
    let t, collector = Collector_pegasus_notes.get t in
    let note =
          Pegasus_notes.get_pegasus_note
              ~code ~year ~firstname ~lastname collector
    in
    t, note

let get_grades_in_pegasus ~firstname ~lastname t =
    let firstname = Special_char.lowercase firstname in
    let lastname = Special_char.lowercase lastname in
    let t, collector = Collector_pegasus_notes.get t in
    let note =
        Pegasus_notes.get_pegasus_notes
          ~firstname ~lastname collector
    in
    t, note

let get_course_exception ~codegps ~year t =
    let t, collector = Collector_course_exceptions.get t in
    let course_exception_opt =
        Course_exceptions.get_course_exception
          ~codegps ~year
          collector
    in
    t, course_exception_opt

module Collector_departements =
  Make_collector_with_unification
    (struct
      type entry = Public_data.dpt
      type collector = Departments.t
      let prefix = get_study_gen
      let repository t = t.parameters.repository_for_departements
      let get data = data.dpts
      let set dpts data = {data with dpts}
      let add = Departments.add_dpt
     end:Interface_collector_with_unification
    with type entry = Public_data.dpt and type collector = Departments.t)

let get_dpt ~acronym t = t, Departments.get_dpt ~acronym t.data.dpts


let get_monitoring_list_prefix t =
    t, t.parameters.repository_for_tuteurs

let get_monitoring_list_repository t =
  get_rep_gen get_bdd get_monitoring_list_prefix t

let get_cost_members_prefix t =
    t, t.parameters.repository_for_cost_members

let get_cost_members_repository t =
  get_rep_gen get_study get_cost_members_prefix t

let get_cursus_list_prefix t =
  t, t.parameters.repository_for_cursus

let get_cursus_list_repository t =
  get_rep_gen get_study get_cursus_list_prefix t

let get_inscriptions_list_prefix t =
    t, t.parameters.repository_for_inscriptions

let get_inscriptions_list_repository t =
    get_rep_gen get_bdd get_inscriptions_list_prefix t

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

let get_sorted_courses_list_prefix t =
      t, t.parameters.repository_for_sorted_courses
let get_sorted_courses_list_repository t =
      get_rep_gen get_bdd get_sorted_courses_list_prefix t


let get_modified_grades_list_prefix t =
  t, t.parameters.repository_for_grades_to_modify
let get_modified_grades_list_repository t =
  get_rep_gen get_bdd get_modified_grades_list_prefix t

let get_csv_separator t = t, Some ','

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

let list_dpt =
  [
    "ibens";"IBENS";"DI";"di";"DMA";"dma";"PHYS";"Phys";"phys";"chimie";
    "gsc";"geos";"geosciences";"géos";"géosciences" ]

let get_cmd_options () =
  let a = Sys.argv in
  let n = Array.length a in
  let rec aux k sol =
    if k < 1 then sol
    else
      aux (k-1) (a.(k)::sol)
  in
  aux (n-1) []

let split_dpt =
  List.partition
    (fun p ->
       List.mem p list_dpt)

let set_home parameters =
    let l = parameters.homelist in
    let rec aux parameters l =
        match l with [] -> failwith "Failed to find a valid home"
           | h::t ->
              if try Sys.is_directory h with _ -> false then {parameters with home = h}
              else aux parameters t
    in aux parameters l

let add_cloud_replist parameters =
    let l =
      List.fold_left
        (fun l home ->
          List.fold_left
            (fun l suffix -> (home^suffix)::l)
            l (List.rev parameters.potential_cloud_suffix))
        parameters.potential_cloud_repositories parameters.homelist
    in
    {parameters with potential_cloud_repositories=l}

let get_option parameters =
  let l = get_cmd_options () in
  let parameters = set_home parameters in
  let parameters = add_cloud_replist parameters in
  let dpt, others = split_dpt l in
  let parameters =
    match dpt with
    | h::_ ->
      begin
        match String.lowercase_ascii h with
        | "ibens" -> set_ibens parameters
        | "dma" -> set_dma parameters
        | "phys" -> set_phys parameters
        | "chimie" -> set_chimie parameters
        | "gsc" | "geos" | "geosciences" | "géos" | "géosciences" -> set_geosciences parameters
        | _ -> parameters
      end
    | [] -> parameters
  in
  let target =
    match others with
    | [] -> None
    | h::_ ->  Some h
  in
  {parameters with target}

let get_option_quick parameters =
  get_option parameters

let get_option t =
  let t =
    open_event_opt
      (Some Profiling.Initialisation)
      t
  in
  let parameters = get_option t.parameters in
  let t =
    close_event_opt
      (Some Profiling.Initialisation)
      t
  in
  {t with parameters}

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
  let rep = parameters.home^parameters.tmp_error_repository in
  let file = parameters.error_log_file  in
  let error_logger =
    let fic = open_out (Printf.sprintf "%s/%s" rep file) in
    Some
      (Loggers.open_logger_from_channel
         ~mode:Loggers.TXT
         fic)
  in
  let rep = parameters.home^parameters.tmp_profiling_repository in
  let file = parameters.profiling_log_file in
  let profiling_logger =
    let fic = open_out (Printf.sprintf "%s/%s" rep file) in
    Some
      (Loggers.open_logger_from_channel
         ~mode:Loggers.HTML_Tabular
         fic)
  in
  let data = empty_data in
  let copy_stack = [] in
  let prefix = "" in
  let cloud_repository = None in
  let t =
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
      copy_stack ;
      cloud_repository ;
    }
  in
  let t = get_option t in
  t


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
  let tmp_rep = t.parameters.home^t.parameters.tmp_error_repository in
  let file = t.parameters.error_log_file  in
  let output_rep = t.parameters.error_log_repository in
  let t = copy __POS__ mk cp t tmp_rep main_rep output_rep file in
  let tmp_rep = t.parameters.home^t.parameters.tmp_profiling_repository in
  let file = t.parameters.profiling_log_file in
  let output_rep = t.parameters.profiling_log_file_repository in
  let t = copy __POS__ mk cp t tmp_rep main_rep output_rep file in
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

let fprintf_verbatim ?logger t x =
  Loggers.fprintf_verbatim
    (which_logger ?logger t)
    x

let breakpage ?logger t =
  Loggers.breakpage (which_logger ?logger t)

let log ?logger ?backgroundcolor ?textcolor ?lineproportion t x =
  Loggers.log
    (which_logger ?logger t)
    ?backgroundcolor ?textcolor ?lineproportion
    x

let warn_and_log
    ?logger ?backgroundcolor
    ?textcolor ?lineproportion
    pos msg exn t
  =
  let () =
    log
      ?logger ?backgroundcolor ?textcolor  ?lineproportion t "%s" msg
  in
  warn pos msg exn t

let log_to_string ?logger ?backgroundcolor ?textcolor ?lineproportion t x =
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

let open_array pos ?colortitle ?makecell ?logger ~with_lines ?size ?color ?bgcolor ?align ~title t =
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
  let error = Loggers.open_array ?makecell ?colortitle ?size ?color ?bgcolor ?align ~title logger in
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

  let std_logger =
    Loggers.open_logger_from_formatter (Format.std_formatter)


  let stop pos message exn t =
    let t = warn pos message exn t in
    let t =
        print_errors "" t
    in
    let t =
        print_errors ~logger:std_logger "" t
    in
    let () = exit 1 in
    t

type save_logger = Loggers.t option
let save_std_logger t = t.std_logger
let restore_std_logger t std_logger = {t with std_logger}


let get_comma_symbol t =
  t,t.parameters.comma_symbol


let close_logger ?logger t =
  let log = which_logger ?logger t in
  let () = Loggers.close_logger log in
  let t,log' = get_std_logger t in
  if log == log'
  then
    {t with std_logger=None}
  else
    t


let get_cost_members data = data.cost_members
let get_cost_members t = lift_get get_cost_members t
let set_cost_members cost_members data = {data with cost_members}
let set_cost_members cost_members t = lift_set set_cost_members cost_members t

let get_scholarships data = data.scholarships
let get_scholarships t = lift_get get_scholarships t
let set_scholarships scholarships data = {data with scholarships}
let set_scholarships scholarships t =
  lift_set set_scholarships scholarships t

let get_pegasus_stages data = data.stages_pegasus
let get_pegasus_stages t = lift_get get_pegasus_stages t
let set_pegasus_stages stages_pegasus data = {data with stages_pegasus}
let set_pegasus_stages stages_pegasus t = lift_set set_pegasus_stages stages_pegasus t

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

let get_inscriptions data = data.inscriptions
let get_inscriptions t = lift_get get_inscriptions t
let set_inscriptions inscriptions data = {data with inscriptions}  let set_inscriptions inscriptions t =
    lift_set set_inscriptions inscriptions  t

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

let get_additional_course data = data.additional_courses
let get_additional_course t =
  lift_get get_additional_course t
let set_additional_course additional_courses data =
    {data with additional_courses}
let set_additional_course additional_courses t =
  lift_set set_additional_course additional_courses t


let get_sorted_courses data = data.sorted_courses
let get_sorted_courses t =
    lift_get get_sorted_courses t
let set_sorted_courses sorted_courses data =
    {data with sorted_courses}
let set_sorted_courses sorted_courses t =
        lift_set set_sorted_courses sorted_courses t

let get_notes_a_modifier data = data.notes_a_modifier
let get_notes_a_modifier t =
  lift_get get_notes_a_modifier t
let set_notes_a_modifier notes_a_modifier data =
  {data with notes_a_modifier}
let set_notes_a_modifier notes_a_modifier t =
      lift_set set_notes_a_modifier notes_a_modifier t

let get_gen pos fetch
      ~firstname
      ~lastname
      ~year  t =
  let firstname = simplify firstname in
  let lastname = simplify lastname in
  let t,l = Collector_administrative_status.find_list
                    ~firstname ~lastname ~year t in
        match l with
            | [] -> warn pos
    (Format.sprintf "Student not found/or multiple students found in Pegasus (%s/%s/%s)" firstname lastname year) Exit t, None
            | h::q ->
    let rep = fetch h in
    if List.for_all (fun elt -> fetch elt = rep) q
    then t, Some rep
    else
    warn pos
      (Format.sprintf "Student not found/or multiple students found in Pegasus (%s/%s/%s)" firstname lastname year) Exit t, None

let get_origine = get_gen __POS__ (fun a -> a.Public_data.pegasus_origin)
let get_gender = get_gen __POS__ (fun a -> a.Public_data.pegasus_gender)
let get_birth_date = get_gen __POS__ (fun a -> a.Public_data.pegasus_birthdate)
let get_birth_city_fr = get_gen __POS__ (fun a -> a.Public_data.pegasus_birth_city_fr)
let get_birth_country_fr = get_gen __POS__ (fun a -> a.Public_data.pegasus_birth_country_fr)
let get_ine_number = get_gen __POS__ (fun a -> a.Public_data.pegasus_ine)
let get_produit_code = get_gen __POS__ (fun a -> a.Public_data.pegasus_produit_de_formation)

let add_cost_member = add_gen_list get_cost_members set_cost_members

let add_pegasus_stage =
  add_gen_unify get_pegasus_stages set_pegasus_stages Pegasus_stages.add_pegasus_stage

let get_pegasus_stages ~firstname ~lastname t =
  let stages_list =
    Pegasus_stages.get_pegasus_stages ~firstname ~lastname t.data.stages_pegasus
  in
  t, stages_list

let add_scholarship =
  add_gen_unify get_scholarships set_scholarships Scholarships.add_scholarship


let get_scholarship ~firstname ~lastname ~current_year t =
  let scholarship_list =
    Scholarships.get_scholarship ~firstname ~lastname t.data.scholarships
  in
  let scholarship_list =
    List.filter
      (fun a ->
      (match a.Public_data.funding_begin with
         | None -> true
         | Some a -> int_of_string a <= int_of_string current_year)
         &&
         (match a.Public_data.funding_end with
          | None -> true
          | Some a -> int_of_string current_year <= int_of_string a) )
      scholarship_list
  in
  match scholarship_list with
  | [] -> t, None
  | [a] -> t, Some a
  | a::_ ->
    let t =
      warn
        __POS__
        (Format.sprintf "Several funding for %s %s in %s" firstname lastname current_year)
        Exit
        t
    in
    t, Some a


let add_mentoring =
  add_gen_unify get_mentoring set_mentoring Mentoring.add_mentoring

let get_mentoring_list
    ?firstname
    ?lastname
    ?tuteur_firstname
    ?tuteur_lastname
    ?year
    t =
  t, Mentoring.get_mentoring_list ?year ?firstname ?lastname ?tuteur_lastname ?tuteur_firstname (get_mentoring t)

let get_mentoring ~firstname ~lastname ~year ?tuteur_gps t =
    let t = 
      warn 
        __POS__ 
        (Format.sprintf "GET MENTORING %s %s %s" firstname lastname year) 
        Exit t         
    in 
    let mentoring_opt =
      Mentoring.get_mentoring
        ~firstname ~lastname ~year (get_mentoring t)
    in
    match mentoring_opt with
    | Some a ->
      t, Some a
    | None ->
      t, tuteur_gps

module Translate_courses =
    Make_collector_translation
      (struct
          type entry = Public_data.course_entry
          let build ?fr ?en gps_entry =
              let french_entry = fr in
              let english_entry = en in
              {Public_data.gps_entry ;
                     Public_data.french_entry ;
                     Public_data.english_entry}
          module Collector =
            (struct
                type entry = Public_data.course_entry
                type collector = Course_name_translation.tentry
                let prefix t = get_repository_to_dump_reports_gen t
                let repository t =
                   t.parameters.repository_for_course_entry
                let get data = data.course_entries
                let set course_entries data = {data with course_entries}
                let add = Course_name_translation.add_course_entry
            end)

          module Report =
          (struct
              type entry = Public_data.course_entry
              type collector = Course_name_translation.tentry
              let prefix t = get_repository_to_dump_reports_gen t
              let repository t = t.parameters.repository_to_dump_course_entries_report
              let get data = data.course_entries_report
              let set course_entries_report data = {data with course_entries_report}
              let add = Course_name_translation.add_course_entry;
          end)

          module Missing =
          (struct
              type entry = Public_data.course_entry
              let prefix t = get_repository_to_dump_missing_gen t
              let repository t = t.parameters.repository_to_dump_missing_course_entries
              let get data = data.missing_course_entries
              let set missing_course_entries data = {data with missing_course_entries}
          end)

          let report_to_list = Course_name_translation.to_list
          let find_opt = Course_name_translation.get_course_entry
          let get_french a = a.Public_data.french_entry
          let get_english a = a.Public_data.english_entry
       end:Interface_translation
        with type entry = Public_data.course_entry
        and type Collector.collector = Course_name_translation.tentry
        and type Report.entry = Public_data.course_entry
        )



(*let get_course_entries_report t =
  let map = get_course_entries_report t in
  t,Course_name_translation.to_list map*)

let add_cursus =
  add_gen_unify get_cursus set_cursus Cursus.add_cursus

let get_cursus ?firstname ?lastname ~year ~level ?dpt ~gpscodelist pos t =
    let rec aux l =
        match l with
        | [] -> None
        | gpscode::tail ->
          match
            Cursus.get_cursus ~year ~level ?dpt ~gpscode (get_cursus t)
          with
          | None -> aux tail
          | x -> x
      in
      let cursus_opt = aux gpscodelist in
      match cursus_opt with
      | Some a -> t, Some a
      | None ->
        match Cursus.get_cursus ~year ~level ?dpt (get_cursus t)
        with
        | Some a -> t, Some a
        | None ->
          if level = "" then t, None
          else let msg =
            Format.sprintf
              "Pas de cursus pour %s%s en %s dans les fichiers du département pour %s %s"
              level
              (match dpt with
             | None -> ""
             | Some i ->
                  Format.sprintf " %s"
                        (Public_data.string_of_dpt i))
              year
              (match firstname with None -> "" | Some x -> x)
              (match lastname with None -> "" | Some x -> x)

            in
            warn pos msg Exit t,
            None

let add_inscription =
      add_gen_unify
        get_inscriptions
        set_inscriptions
        Inscriptions.add_inscription

let get_inscription ~year ~level ?dpt ~lastname ~firstname t =
  t,
  Inscriptions.get_inscription
    ~year ~level ?dpt ~lastname ~firstname  (t.data.inscriptions)

let get_cursus ~year ~level ?dpt ~gpscodelist ?firstname ?lastname pos t =
  let t, univ_opt =
    match firstname, lastname with
  | _, None | None,_ -> t, None
  | Some firstname, Some lastname ->
      get_inscription ~year ~level ?dpt ~lastname ~firstname t
  in
  let t, cursus_opt = get_cursus ?firstname ?lastname ~year ~level ?dpt ~gpscodelist pos t in
  match univ_opt with
     | None -> t, cursus_opt
     | Some a ->
    match a.Public_data.inscription_univ with
      | None -> t, cursus_opt
      | Some _ ->
      let cursus =
       match cursus_opt with
       | None -> Public_data.empty_cursus
       | Some a -> a
      in
      let cursus_univ = a.Public_data.inscription_univ in
      let cursus = {cursus with Public_data.cursus_univ} in
      t, Some cursus


let add_program =
  add_gen_unify get_programs set_programs Programs.add_program

let get_program ~code_gps t =
  let program_opt =
    Programs.get_program ~code_gps  t.data.programs
  in
  t, program_opt

let add_dispense =
    add_gen_unify_warn
      get_dispenses
      set_dispenses
      Dispenses.add_dispense

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
    get_additional_course set_additional_course Cours_a_ajouter.add_additional_course

let get_additional_course
    ~firstname ~lastname
    t =
  t,
  Cours_a_ajouter.get_additional_courses
    ~firstname ~lastname
    t.data.additional_courses

let add_sorted_course =
    add_gen_unify
        get_sorted_courses set_sorted_courses Cours_a_trier.add_sorted_course

let get_sorted_courses
      ?firstname ?lastname
      ?year
      ?libelle
      ?codegps
      t =
      let courses_opt =
        Cours_a_trier.get_sorted_courses
                ?firstname ?lastname
                ?year
                ?libelle
                ?codegps
                t.data.sorted_courses
            in
            t, courses_opt

let get_sorted_internships
      ?firstname ?lastname
      ?year
      ?libelle
      t =
      let internships_opt =
                    Stages_a_trier.get_sorted_internships
                            ?firstname ?lastname
                            ?year
                            ?libelle
                            t.data.sorted_internships
                        in
                        t, internships_opt

let add_note_a_modifier =
  add_gen_unify_warn
    get_notes_a_modifier
    set_notes_a_modifier
    Notes_a_modifier.add_note_a_modifier

let get_truc_a_modifier
    get
    ~firstname ~lastname ~code ~year
    t =
  let note_elt_opt =
    Notes_a_modifier.get_note_a_modifier
      ~firstname ~lastname ~code ~year
      t.data.notes_a_modifier
  in
  match note_elt_opt with
  | None -> t, None
  | Some note_elt ->
    let truc = get note_elt in
    t, truc

let get_note_a_modifier
    ~firstname ~lastname ~code ~year
    t
  =
  get_truc_a_modifier
    (fun a -> a.Public_data.notetm_note)
    ~firstname ~lastname ~code ~year
    t

let get_ects_a_modifier
    ~firstname ~lastname ~code ~year
    t =
    get_truc_a_modifier
      (fun a -> a.Public_data.notetm_ects)
      ~firstname ~lastname ~code ~year
      t

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

let get_commission t =
  t, t.parameters.commission

let get_main_commission_rep t =
  t, t.parameters.commissions_repository

let get_pictures_prefix t =
  t, t.parameters.repository_to_access_pictures

let get_picture_file_names_mention_promotion t =
  t, t.parameters.picture_file_names_mention_promotion

let get_pictures_stored_according_to_promotions t =
  t, t.parameters.pictures_stored_according_to_promotions

let get_all_potential_pictures_repository t =
  let t, l = get_all_potential_local_repositories t in
  let t, rep = get_pictures_prefix t in
  t,
  List.rev_map
    (fun local ->
       match local, rep with
        | "", "" -> ""
        | a,"" | "",a -> a
        | a,b -> Format.sprintf "%s/%s" a b)
    (List.rev l)

    let get_all_write_potential_pictures_repository t =
      let t, l = get_local_repository t in
      let t, rep = get_pictures_prefix t in
      t,
      List.rev_map
        (fun local ->
           match local, rep with
            | "", "" -> ""
            | a,"" | "",a -> a
            | a,b -> Format.sprintf "%s/%s" a b)
        [l]

let get_picture_potential_locations
    ~firstname ~lastname ~year t =
  let t, l = get_all_potential_pictures_repository t in
  let t, b =
    get_pictures_stored_according_to_promotions t
  in
  let t,b2 =
    get_picture_file_names_mention_promotion t
  in
    List.fold_left
      (fun (t,l) rep ->
         let rep =
           if b then
             if rep = "" then year
             else
               Format.sprintf "%s/%s" rep year
           else
             rep
         in
         let yearprefix =
           if b2 then year else ""
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
         t, ((base^".jpg")::(base^".pdf")::l))
      (t,[]) (List.rev l)

let get_picture_write_potential_locations
          ~firstname ~lastname ~year t =
        let t, l = get_all_write_potential_pictures_repository t in
        let t, b =
          get_pictures_stored_according_to_promotions t
        in
        let t,b2 =
          get_picture_file_names_mention_promotion t
        in
          List.fold_left
            (fun (t,l) rep ->
               let rep =
                 if b then
                   if rep = "" then year
                   else
                     Format.sprintf "%s/%s" rep year
                 else
                   rep
               in
               let yearprefix =
                 if b2 then year else ""
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
               t, ((base^".jpg")::(base^".pdf")::l))
            (t,[]) (List.rev l)

let get_target t = t, t.parameters.target

let list_all_cursus t =
  let () =
    Public_data.LevelMap.iter
      (fun level ->
         Public_data.DptOptMap.iter
           (fun dpt_opt ->
              Public_data.YearMap.iter
                (fun year ->
                    Public_data.CodeOptMap.iter
                      (fun codeopt cursus ->
                        Format.printf
                          "%s %s %s %s -> %s %s %s %s @ "
                          level
                          (match dpt_opt with
                          | None -> "None"
                          | Some x -> Public_data.string_of_dpt x)
                          year
                          (match codeopt with
                          | None -> ""
                          | Some a -> Format.sprintf "(%s)" a)
                          cursus.Public_data.cursus_niveau
                          (match cursus.Public_data.cursus_dpt with
                          | None -> "None"
                          | Some a -> (Public_data.string_of_dpt a)
                          )
                          cursus.Public_data.cursus_annee_academique
                          (match cursus.Public_data.cursus_gps with
                          | None -> ""
                          | Some a -> Format.sprintf "(%s)" a)
              ))))
      t.data.cursus
  in
  Format.print_flush ()

let get_ENSPSL_logo t =
  let t, local = get_all_potential_local_repositories t in
  let logo = t.parameters.enspsl_logo in
  t, List.rev_map
    (fun local ->
      if local = ""
      then
        logo
      else
        Format.sprintf "%s/%s" local logo)
    (List.rev local)

    let get_ENSPSL_logo_bis t =
      let t, local = get_all_potential_local_repositories t in
      let logo = t.parameters.enspsl_logo_bis in
      t, List.rev_map
        (fun local ->
          if local = ""
          then
            logo
          else
            Format.sprintf "%s/%s" local logo)
        (List.rev local)

let get_promo ~firstname ~lastname t =
  let t, list = Student_ids.get t in
  let firstname = simplify firstname in
  let lastname = simplify lastname in
  let list =
    List.filter
      (fun a ->
         simplify a.Public_data.firstname = firstname
         && simplify a.Public_data.lastname = lastname)
      list
  in
  match list with
  | [] ->
    warn
      __POS__
      (Format.sprintf
         "Unknown student %s %s" firstname lastname )
         Exit
         t,
    None
  | _::_::_ ->
    warn
      __POS__
      (Format.sprintf
         "Several students %s %s" firstname lastname )
      Exit
      t,
    None
  | [a] ->
    t, a.Public_data.promotion

let get_promos_personnal_repository t =
  let t, cloud_rep = get_cloud_repository t in
  let main_rep = t.parameters.scholarships_repository in
  let rep =
    match main_rep with
    | "" -> Printf.sprintf "%s" cloud_rep
    | _ -> Printf.sprintf "%s/%s" cloud_rep main_rep
  in
  t, rep

let get_promo_personnal_repository ~promo t =
  let t, cloud_rep = get_cloud_repository t in
  let main_rep = t.parameters.scholarships_repository in
  let promo =
    Printf.sprintf "Promo%s" promo
  in
  let rep =
    match main_rep with
    | "" -> Printf.sprintf "%s/%s" cloud_rep promo
    | _ -> Printf.sprintf "%s/%s/%s" cloud_rep main_rep promo
  in
  t, rep

let get_student_personnal_repository ?promo ~firstname ~lastname t =
  let t, promo_rep =
    match promo with
    | Some promo ->
      get_promo_personnal_repository ~promo t
    | None ->
      get_cloud_repository t
  in
  let fiche = t.parameters.repository_to_dump_transcripts in
  let f_firstname =
    if false then
      (fun x -> Special_char.correct_string_filename
          (Special_char.capitalize x))
    else
      (fun x -> Special_char.correct_string_filename (Special_char.capitalize (Special_char.correct_string x)))
  in
  let f_lastname =
    if false then
      (fun x -> Special_char.correct_string_filename
          (Special_char.uppercase x))
    else
      (fun x ->
         Special_char.correct_string_filename
           (Special_char.uppercase
              (Special_char.correct_string x)))
  in
  let lastname = f_lastname lastname in
  let firstname = f_firstname firstname in
  let rep =
    match promo_rep, fiche with
    | "","" -> Printf.sprintf "%s.%s/" lastname firstname
    | _,"" -> Printf.sprintf "%s/%s.%s/" promo_rep lastname firstname
    | "",_ ->
      Printf.sprintf "%s.%s/%s" lastname firstname
        fiche
    | _,_ ->
      Printf.sprintf "%s/%s.%s/%s" promo_rep  lastname
        firstname fiche
  in
  t, rep

let get_students_personnal_files_fr  ~promo t =
  let promo_string = promo in
  let promo = Some promo in
  let t, rep =
    get_student_personnal_repository ?promo ~firstname:"*" ~lastname:"*" t
  in
  t, Format.sprintf "%s/%s*.pdf" rep promo_string

let get_students_personnal_files_en ~promo t =
  let promo_string = promo in
  let promo = Some promo in
  let t, rep =
    get_student_personnal_repository ?promo ~firstname:"*" ~lastname:"*" t
  in
  t, Format.sprintf "%s/%s*.en.pdf" rep promo_string


let get_students_personnal_files ?language ~promo t =
  let language =
    match language with
    | None -> Public_data.French
    | Some l -> l
  in
  match language with
  | Public_data.French -> get_students_personnal_files_fr ~promo t
  | Public_data.English -> get_students_personnal_files_en ~promo t

let get_main_dpt t =
  t, t.parameters.main_dpt

let is_main_dpt_di t =
  let t,dpt = get_main_dpt t in
  t, dpt = Public_data.DI

let is_main_dpt_dma t =
  let t,dpt = get_main_dpt t in
  t, dpt = Public_data.DMA

let is_main_dpt_phys t =
  let t,dpt = get_main_dpt t in
  t, dpt = Public_data.PHYS

let is_main_dpt_chimie t =
  let t,dpt = get_main_dpt t in
  t, dpt = Public_data.CHIMIE

let is_main_dpt_geosciences t =
  let t,dpt = get_main_dpt t in
  t, dpt = Public_data.GEOSCIENCES

let is_main_dpt_ibens t =
  let t,dpt = get_main_dpt t in
  t, dpt = Public_data.IBENS

let is_main_dpt_bio = is_main_dpt_ibens

let get_language t =
  t, t.parameters.language

let get_repartition t =
  t, t.parameters.repartition

let get_file_retriever_skip t =
  t, t.parameters.file_retriever_skip

let kill_file_retriever t =
  let file_retriever_skip = false in
  let parameters = {t.parameters with file_retriever_skip} in
  {t with parameters}

let get_file_retriever_n_fail t =
  t, t.parameters.file_retriever_n_fail

let get_file_retriever_max_fail t =
  t, t.parameters.file_retriever_max_n_fail

let file_retriever_fail t =
  let t,n = get_file_retriever_n_fail t in
  let t,max = get_file_retriever_max_fail t in
  let file_retriever_n_fail = n + 1 in
  let parameters = {t.parameters with file_retriever_n_fail} in
  let t = {t with parameters} in
  if file_retriever_n_fail > max then
    kill_file_retriever t
  else
    t

let get_diplomation_rep ?firstname ?lastname t =
  t,
    let t, cloud = get_cloud_repository t in
    let main = t.parameters.diplomation_repository in
      match t.parameters.diplomation_year with
        | None -> None
        | Some year ->
        let year_i = int_of_string year in
        let year = Format.sprintf "%i-%i" year_i (year_i+1) in
        let rep = Format.sprintf "%s/%s/%s/candidates/" cloud main year in
        match firstname, lastname with
          | None, _ | _,None -> Some rep
          | Some firstname, Some lastname ->
            Some (Format.sprintf "%s%s_%s" rep (Special_char.uppercase (Tools.remove_space_from_string lastname)) (Special_char.capitalize (Tools.remove_space_from_string firstname)))

let get_commission_rep_from_key ?commission_rep ?univ sous_commission_short t =
                let t, commission_rep =
                  match commission_rep with
                  | None -> get_main_commission_rep t
                  | Some commission_rep -> t, commission_rep
                in
                let t, main_rep =
                  get_dated_output_repository t
                in
                let commission_rep =
                  match main_rep,commission_rep with
                  | "",a | a,"" -> a
                  | a,b -> Printf.sprintf "%s/%s" a b
                in
                let sous_commission_short =
                  match univ with
                  | None -> sous_commission_short
                  | Some a -> sous_commission_short^(Public_data.file_suffix_of_univ a)
                in
                t,
                match commission_rep,sous_commission_short with
                  | "",a -> Printf.sprintf "attestations/%s" a,
                            Printf.sprintf "comptes-rendus/%s" a,
                            Printf.sprintf "transcripts/%s" a
                  | a,"" -> Printf.sprintf "%s/attestations" a,
                            Printf.sprintf "%s/comptes-rendus" a,
                            Printf.sprintf "%s/transcripts" a
                  | a,b -> Printf.sprintf "%s/attestations/%s" a b,
                            Printf.sprintf "%s/comptes-rendus/%s" a b,
                            Printf.sprintf "%s/transcripts/%s" a b


let get_commission_rep ?commission_rep ~sous_commission ?univ t =
  let sous_commission_short =
    match
      sous_commission
    with
    | Public_data.Diplome_ENS dip -> dip.Public_data.dens_short
    | Public_data.Diplome_National dip -> dip.Public_data.dn_short
  in
  get_commission_rep_from_key ?commission_rep ?univ sous_commission_short t

let push_copy ~input_rep ~output_rep ~file_name t =
  {t with copy_stack = (input_rep,file_name,output_rep)::t.copy_stack}

let is_empty_copy_stack t = t.copy_stack = []

let pop_copy ~copy t =
  match t.copy_stack with
  | [] ->
    warn __POS__ "Copy stack is empty" Exit t
  | (input_rep,file_name,output_rep)::tail ->
    let t = {t with copy_stack = tail} in
      copy ~input_rep ~file_name ~output_rep t

let rec empty_copy ~copy t =
  if is_empty_copy_stack t then t
  else empty_copy ~copy (pop_copy ~copy t)

let get_is_bilingual t =
  t, t.parameters.bilinguage

let bilingual_string ?english ~french t =
  let t, b = get_is_bilingual t in
  let t, lang = get_language t in
  match b, english, lang with
  | _, None, _  | false,_,Public_data.French -> t, french
  | false, Some english, Public_data.English  -> t, english
  | true, Some english,_ ->
    t, Format.sprintf "\\BiLingual{%s}{%s}" french english

let bi_gen f t ?logger ?english ~french =
  let t, bi = bilingual_string ?english ~french t in
  f ?logger bi t

let log_to_string
    ?logger
    ?backgroundcolor
    ?textcolor
    ?lineproportion ?english t french  =
  let t, bi = bilingual_string ?english ~french t in
  log_to_string ?logger ?backgroundcolor ?textcolor ?lineproportion t bi

let fold_right2 f l l' a = List.fold_left2
    (fun c a b  -> f a b c)
    a
    (List.rev l) (List.rev l')

let open_array pos ?colortitle ?makecell ?logger ~with_lines ?size ?color ?bgcolor ?align ~title ?title_english t  =
  let t, title  =
    if List.length title = (match title_english with None -> -1 | Some l -> List.length l) then
    match title_english with
    | None -> t, title
    | Some a ->
      try
        fold_right2
          (fun fr en (t, list) ->
             let t, h =
               fold_right2
                 (fun french english (t,list) ->
                    let t, bi = bilingual_string ~english ~french t in
                    t, bi::list)
                 fr en (t,[])
             in
             t, h::list)
          title a
          (t,[])
      with
        Not_found ->
        warn
          pos
          "Incompatible arguments (english / french must have the same length)"
          Exit
          t, []
    else
    let t, lang = get_language t in
    match lang, title_english  with Public_data.French,_ | _, None -> t, title
                | Public_data.English, Some title_english -> t, title_english
  in
  open_array pos ?colortitle ?makecell ?logger ~with_lines ?size ?color ?bgcolor ?align ~title t


let print_cell ?logger ?english french t =
  bi_gen print_cell t ?logger ?english ~french

let print_optional_cell ?logger ?english french t =
  bi_gen print_optional_cell t ?logger ?english ~french

let log_string
    ?logger
    ?backgroundcolor
    ?textcolor
    ?lineproportion ?english t french =
  let s =
    log_to_string
      ?logger
      ?backgroundcolor
      ?textcolor
      ?lineproportion ?english t french
  in
  fprintf ?logger t "%s" s

let get_diplomation_year t = t, t.parameters.diplomation_year
let do_we_consider_grades_without_registration t = t, t.parameters.add_grades_without_registration
let do_we_load_gps_data t = t,t.parameters.load_gps_data
let do_we_log_pegasus_entries t  = t,t.parameters.log_pegasus_entries

