type parameters =
  {
    safe_mode: bool;
    main_dpt: Public_data.main_dpt;
    cloud_synchronization_mode: Public_data.cloud_synchronization_mode ;
    cloud_client: Public_data.cloud_client ;
    cloud_client_options: string ;
    potential_cloud_repositories: string list ;
    cloud_support_dynamic_link : bool ;
    pdfgenerator : Public_data.pdf_generator ;
    pdfgenerator_options : string ;
    local_repository: string ;
    scholarships_repository: string ;
    repository_to_dump_transcripts: string ;
    distant_repository: string ;
    machine_to_access_gps: string ;
    url_to_access_annuaire: string ;
    port_to_access_gps: string;
    repository_to_access_gps: string;
    output_repository: string;
    database_repository: string;
    study_repository:string;
    parameters_repository:string;
    gps_backup_repository:string;
    enspsl_logo:string;
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
    repository_for_internship_entry: string;
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
    repository_to_dump_missing_course_name_translation: string;
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
  }


let parameters =
  {
    safe_mode = true;
    main_dpt = Public_data.DI;
    log_mkdir = false;
    cloud_synchronization_mode = Public_data.CommandLine ;
    cloud_client = Public_data.NextCloudCmd ;
    cloud_client_options = "-n --silent" ;
    potential_cloud_repositories = ["/users/absint3/feret/Nextcloud";"/Users/feret/Nextcloud"] ;
    cloud_support_dynamic_link = false ;
    pdfgenerator = Public_data.PdfLatex ;
    pdfgenerator_options = "-interaction=nonstopmode";
    local_repository = "di/suivi_pedagogique" ;
    enspsl_logo = "LOGOs/ENSPSL.png" ;
    scholarships_repository = "di/scolarite/ELEVES" ;
    repository_to_dump_transcripts = "fiches_de_notes" ;
    distant_repository = "https://cloud.di.ens.fr/" ;
    machine_to_access_gps = "violette.ens.fr" ;
    url_to_access_annuaire = "http://annuaireweb.ens.fr/" ;
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
    repository_to_dump_missing_course_name_translation = "cours_non_traduits_par_code_gps";
    repository_to_dump_missing_course_entries = "cours_non_traduits_par_libelle";
    repository_to_dump_course_entries_report = "cours";
    repository_for_bourses = "bourses";
    repository_for_tuteurs = "tuteurs";
    repository_for_cours = "cours";
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
    file_retriever_log_repository = "/users/absint3/feret/tmp" ;
    file_retriever_log_file = "gps_access.log";
    tmp_annuaire_repository = "/users/absint3/feret/tmp";
    include_pictures = true;
    file_retriever_annuaire_html_file = "annuaire.html";
    annuaire_check_certificate = false ;
    file_retriever_time_out_in_seconds = Some 300;
    file_retriever_checking_period_in_seconds = 5;
    file_retriever_skip = false;
    file_retriever_n_fail = 0;
    file_retriever_max_n_fail = 5;
    tmp_profiling_repository = "/users/absint3/feret/tmp";
    profiling_log_file_repository = "profiling_info";
    profiling_log_file = "profiling_info.html";
    tmp_error_repository = "/users/absint3/feret/tmp";
    error_log_repository = "erreurs_internes";
    error_log_file = "error.txt";
    comma_symbol = ',';
    dens_repository = "diplomation";
    diplomation_year = Some "2023" ;
    repository_for_internship_entry = "stages" ;
    repository_for_minors = "mineures" ;
      repository_for_majors = "majeures" ;
    repository_for_dens_candidate = "dens_candidates" ; repository_to_dump_missing_minors = "mineures" ;
    repository_to_dump_missing_majors = "majeures" ;
    repository_to_dump_missing_internship_translation = "stages" ;
    repository_to_dump_dens_candidate = "dens_candidates" ;
    current_academic_year = "2023";
    commissions_repository = "commissions_des_etudes";
    commission = None ;
    target = None ;
    signature = "feret+tampon.pdf";
    bilinguage = true ;
    language  = Public_data.French;
    repartition = Public_data.Annee_de_validation_du_cours;

  }

let _ = parameters.parameters_repository

let set_dma parameters =
  {
    parameters with
    main_dpt = Public_data.DMA ;
    commission = Some ("20 juin 2023",  "2022");
    local_repository = "dma/suivi_pedagogique" ;
    scholarships_repository = "dma/scolarite/ELEVES" ;
    repartition = Public_data.Annee_obtention_du_diplome ;
    include_pictures = false;
  }

let set_phys parameters =
  {
    parameters with
    main_dpt = Public_data.PHYS ;
    commission = None (*Some ("23 juin 2021",  "2020")*);
    local_repository = "phys/suivi_pedagogique" ;
    scholarships_repository = "phys/scolarite/ELEVES" ;
    repartition = Public_data.Annee_obtention_du_diplome ;
    include_pictures = false;
  }

  let set_chimie parameters =
    {
      parameters with
      main_dpt = Public_data.CHIMIE ;
      commission = None (*Some ("23 juin 2021",  "2020")*);
      local_repository = "chimie/suivi_pedagogique" ;
      scholarships_repository = "chimie/scolarite/ELEVES" ;
      repartition = Public_data.Annee_obtention_du_diplome ;
      include_pictures = true;
    }

    let set_geosciences parameters =
      {
        parameters with
        main_dpt = Public_data.GEOSCIENCES ;
        commission = None (*Some ("23 juin 2021",  "2020")*);
        local_repository = "geosciences/suivi_pedagogique" ;
        scholarships_repository = "geosciences/scolarite/ELEVES" ;
        repartition = Public_data.Annee_obtention_du_diplome ;
        include_pictures = true;
      }

type data =
  {
    students: Public_data.student_id list ;
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
    dens_candidates_suggestion: Public_data.dens_candidate list

  }

let empty_data =
  {
    students = [];
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
        {t with cloud_repository = Some head}, head
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
  t.parameters.file_retriever_log_repository

let get_file_retriever_log_file t =
  t,
  t.parameters.file_retriever_log_file

let get_file_retriever_annuaire_tmp_repository t =
  t,
  t.parameters.tmp_annuaire_repository

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

let get_repository_to_dump_under_average_validated_grades t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_under_average_validated_grades)
    t

let get_repository_to_dump_out_of_schooling_years t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_out_of_schooling_years)
    t

let get_repository_to_dump_missing_grades t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_grades
    )
    t

let get_repository_to_dump_missing_course_name_translations t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_course_name_translation
    )
    t

let get_dens_candidate_suggestion_list_repository t =
      get_repository_to_dump_missing_gen
        (fun t ->
           t.parameters.repository_for_dens_candidate
        )
        t

let get_courses_to_be_sorted_list_repository t =
      get_repository_to_dump_missing_gen
        (fun t ->
           t.parameters.repository_for_courses_to_be_sorted)
        t

let get_internships_to_be_sorted_list_repository t =
      get_repository_to_dump_missing_gen
        (fun t ->
            t.parameters.repository_for_internships_to_be_sorted)
        t

let get_repository_to_dump_missing_internship_translations t =
      get_repository_to_dump_missing_gen
        (fun t ->
           t.parameters.repository_to_dump_missing_internship_translation
        )
        t


let get_repository_to_dump_missing_course_entries t =
  get_repository_to_dump_missing_gen
    (fun t ->
       t.parameters.repository_to_dump_missing_course_entries
    )
    t

let get_repository_to_dump_missing_minors t =
    get_repository_to_dump_missing_gen
        (fun t ->
           t.parameters.repository_to_dump_missing_minors
        )
        t

        let get_repository_to_dump_missing_majors t =
            get_repository_to_dump_missing_gen
                (fun t ->
                   t.parameters.repository_to_dump_missing_majors
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

let get_repository_to_dump_courses_validated_twice t =
      get_repository_to_dump_missing_gen
        (fun t ->
           t.parameters.repository_to_dump_courses_validated_twice)
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

let get_repository_to_dump_course_entries_report t =
  get_repository_to_dump_reports_gen
    (fun t ->
       t.parameters.repository_to_dump_course_entries_report)
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

  let get_repository_to_dump_dens_supplement ?output_repository t =
    let t, output =
      match output_repository with
      | Some rep -> t, rep
      | None -> get_dated_output_repository t
    in
    match
      output,
      t.parameters.dens_repository
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


let get_backup_repository t =
  let t, local = get_local_repository t in
  match local, t.parameters.gps_backup_repository with
  | "", "" -> t, ""
  | a,"" | "",a -> t, a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_repository_for_handmade_gps_files t =
  let t, rep =
    get_backup_repository t
  in
  match
    rep,
    t.parameters.repository_for_handmade_gps_files
  with
  | "","" -> t,""
  | "",a | a,"" -> t,a
  | a,b -> t, Format.sprintf "%s/%s" a b

let get_repository_for_backup_gps_files t =
  let t, rep =
    get_backup_repository t
  in
  match
    rep,
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

let get_dens_candidates_list_prefix t =
    t, t.parameters.repository_to_dump_dens_candidate

let get_stage_entry_list_prefix t =
  t, t.parameters.repository_for_internship_entry

let get_mineure_entry_list_prefix t =
  t, t.parameters.repository_for_minors

  let get_majeure_entry_list_prefix t =
    t, t.parameters.repository_for_majors

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

let get_stage_entry_list_repository t =
  get_rep_gen get_bdd get_stage_entry_list_prefix t

let get_mineure_entry_list_repository t =
  get_rep_gen get_bdd get_mineure_entry_list_prefix t

let get_majeure_entry_list_repository t =
  get_rep_gen get_bdd get_majeure_entry_list_prefix t

let get_dens_candidates_list_repository t =
    get_rep_gen get_bdd get_dens_candidates_list_prefix t

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

let get_course_entry_list_prefix t =
          t, t.parameters.repository_for_course_entry

let get_course_entry_list_repository t =
  get_rep_gen get_bdd get_course_entry_list_prefix t

let get_departments_list_prefix t =
       t, t.parameters.repository_for_departements

let get_departments_list_repository t =
  get_rep_gen get_study get_departments_list_prefix t

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

let get_sorted_internships_list_prefix t =
      t, t.parameters.repository_for_sorted_internships
let get_sorted_internships_list_repository t =
      get_rep_gen get_bdd get_sorted_internships_list_prefix t

let get_modified_grades_list_prefix t =
  t, t.parameters.repository_for_grades_to_modify
let get_modified_grades_list_repository t =
  get_rep_gen get_bdd get_modified_grades_list_prefix t

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

let list_dpt =
  [
    "DI";"di";"DMA";"dma";"PHYS";"Phys";"phys";"chimie";
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

let _get_dpt =
  List.filter
    (fun p ->
       List.mem p list_dpt)

let _get_others =
  List.filter
    (fun p ->
       not (List.mem p list_dpt))

let split_dpt =
  List.partition
    (fun p ->
       List.mem p list_dpt)

let get_option parameters =
  let l = get_cmd_options () in
  let dpt, others = split_dpt l in
  let parameters =
    match dpt with
    | h::_ ->
      begin
        match String.lowercase_ascii h with
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

let simplify s =
  Special_char.lowercase
    (Special_char.correct_string_txt
       (String.trim s))


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



let get_data t = t.data
let set_data data t = {t with data}
let lift_get get t = get (get_data t)
let lift_set set data t =
  set_data (set data (get_data t)) t

let get_students data = data.students
let get_students t = lift_get get_students t
let set_students students data = {data with students}
let set_students students t = lift_set set_students students t

let get_dens_candidates data = data.dens_candidates
let get_dens_candidates t = lift_get get_dens_candidates t
let set_dens_candidates dens_candidates data = {data with dens_candidates}
let set_dens_candidates dens_candidates t =
  lift_set set_dens_candidates dens_candidates t

let get_minors data = data.minors
let get_minors t = lift_get get_minors t
let set_minors minors data = {data with minors}
let set_minors minors t =
  lift_set set_minors minors t

let get_majors data = data.majors
let get_majors t = lift_get get_majors t
let set_majors majors data = {data with majors}
let set_majors majors t =
    lift_set set_majors majors t


let get_dens_candidates_suggestion data = data.dens_candidates_suggestion
let get_dens_candidates_suggestion t = lift_get get_dens_candidates_suggestion t
let set_dens_candidates_suggestion dens_candidates_suggestion data = {data with dens_candidates_suggestion}
let set_dens_candidates_suggestion dens_candidates_suggestion t =
    lift_set set_dens_candidates_suggestion dens_candidates_suggestion t

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

let get_course_entries data = data.course_entries
let get_course_entries t = lift_get get_course_entries t
let set_course_entries course_entries data = {data with course_entries}
let set_course_entries course_entries t =
        lift_set set_course_entries course_entries t

let get_cursus data = data.cursus
let get_cursus t = lift_get get_cursus t
let set_cursus cursus data = {data with cursus}
let set_cursus cursus t =
  lift_set set_cursus cursus t

  let get_inscriptions data = data.inscriptions
  let get_inscriptions t = lift_get get_inscriptions t
  let set_inscriptions inscriptions data = {data with inscriptions}
  let set_inscriptions inscriptions t =
    lift_set set_inscriptions inscriptions  t

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

let get_minor_suggestions data = data.minor_suggestion
let get_minor_suggestions t = lift_get get_minor_suggestions t
let set_minor_suggestions minor_suggestion data =
    {data with minor_suggestion}
let set_minor_suggestions minor_suggestions t =
    lift_set set_minor_suggestions minor_suggestions t

let get_major_suggestions data = data.major_suggestion
let get_major_suggestions t = lift_get get_major_suggestions t
let set_major_suggestions major_suggestion data =
    {data with major_suggestion}
let set_major_suggestions major_suggestions t =
    lift_set set_major_suggestions major_suggestions t


let get_non_accepted_grades data = data.non_accepted_grades
let get_non_accepted_grades t = lift_get get_non_accepted_grades t
let set_non_accepted_grades non_accepted_grades data =
  {data with non_accepted_grades}
let set_non_accepted_grades non_accepted_grades t =
  lift_set set_non_accepted_grades non_accepted_grades t

let get_out_of_schooling_years data = data.out_of_schooling_years
let get_out_of_schooling_years t = lift_get get_out_of_schooling_years t
let set_out_of_schooling_years out_of_schooling_years data =
    {data with out_of_schooling_years}
let set_out_of_schooling_years out_of_schooling_years t =
    lift_set set_out_of_schooling_years out_of_schooling_years t

let get_under_average_validated_grades data = data.under_average_validated_grades
let get_under_average_validated_grades t = lift_get get_under_average_validated_grades t
let set_under_average_validated_grades under_average_validated_grades data =
    {data with under_average_validated_grades}
let set_under_average_validated_grades under_average_validated_grades t =
    lift_set set_under_average_validated_grades under_average_validated_grades t

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

let get_courses_validated_twice data = data.courses_validated_twice
let get_courses_validated_twice t = lift_get get_courses_validated_twice t
let set_courses_validated_twice courses_validated_twice data =
    {data with courses_validated_twice}
let set_courses_validated_twice courses_validated_twice t =
    lift_set set_courses_validated_twice courses_validated_twice t

let get_missing_internship_descriptions data =
  data.missing_internship_descriptions
let get_missing_internship_descriptions t = lift_get get_missing_internship_descriptions t
let set_missing_internship_descriptions missing_internship_descriptions data =
  {data with missing_internship_descriptions}
let set_missing_internship_descriptions missing_internship_descriptions t =
  lift_set set_missing_internship_descriptions missing_internship_descriptions t

let get_missing_internship_translations data =
    data.missing_internship_translations
let get_missing_internship_translations t =
    lift_get get_missing_internship_translations t
let set_missing_internship_translations missing_internship_translations data =
    {data with missing_internship_translations}
let set_missing_internship_translations missing_internship_translations t =
    lift_set set_missing_internship_translations missing_internship_translations t



let get_missing_course_entries data = data.missing_course_entries
let get_missing_course_entries t = lift_get get_missing_course_entries t
let set_missing_course_entries missing_course_entries data =
  {data with missing_course_entries}
let set_missing_course_entries missing_course_entries t =
  lift_set set_missing_course_entries missing_course_entries t

let get_course_entries_report data = data.course_entries_report
let get_course_entries_report t = lift_get get_course_entries_report t
let set_course_entries_report course_entries_report data =
    {data with course_entries_report}
let set_course_entries_report course_entries_report t =
    lift_set set_course_entries_report course_entries_report t

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


let get_courses_to_be_sorted data = data.courses_to_be_sorted
let get_courses_to_be_sorted t =
    lift_get get_courses_to_be_sorted t
let set_courses_to_be_sorted courses_to_be_sorted data =
      {data with courses_to_be_sorted}
let set_courses_to_be_sorted courses_to_be_sorted t =
    lift_set set_courses_to_be_sorted courses_to_be_sorted t

let get_sorted_courses data = data.sorted_courses
let get_sorted_courses t =
    lift_get get_sorted_courses t
let set_sorted_courses sorted_courses data =
    {data with sorted_courses}
let set_sorted_courses sorted_courses t =
        lift_set set_sorted_courses sorted_courses t

let get_internships_to_be_sorted data = data.internships_to_be_sorted
let get_internships_to_be_sorted t =
    lift_get get_internships_to_be_sorted t
let set_internships_to_be_sorted internships_to_be_sorted data =
    {data with internships_to_be_sorted}
let set_internships_to_be_sorted internships_to_be_sorted t =
    lift_set set_internships_to_be_sorted internships_to_be_sorted t

let get_sorted_internships data = data.sorted_internships
let get_sorted_internships t =
    lift_get get_sorted_internships t
let set_sorted_internships sorted_internships data =
    {data with sorted_internships}
let set_sorted_internships sorted_internships t =
    lift_set set_sorted_internships sorted_internships t


let get_notes_a_modifier data = data.notes_a_modifier
let get_notes_a_modifier t =
  lift_get get_notes_a_modifier t
let set_notes_a_modifier notes_a_modifier data =
  {data with notes_a_modifier}
let set_notes_a_modifier notes_a_modifier t =
      lift_set set_notes_a_modifier notes_a_modifier t

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

let add_dens_candidate unify =
  add_gen
    get_dens_candidates
    set_dens_candidates
    (Dens_candidates.add_dens_candidate unify)

let add_dens_minor unify =
    add_gen
      get_minors
      set_minors
        (Minor_candidates.add_minor_candidate unify)

let add_dens_major unify =
    add_gen
      get_majors
      set_majors
      (Major_candidates.add_major_candidate unify)


let add_scholarship unify =
  add_gen
    get_scholarships
    set_scholarships
    (Scholarships.add_scholarship unify)

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


let add_mentoring unify =
  add_gen
    get_mentoring
    set_mentoring
    (Mentoring.add_mentoring unify)

let get_mentoring_list
    ?firstname
    ?lastname
    ?tuteur_firstname
    ?tuteur_lastname
    ?year
    t =
  t, Mentoring.get_mentoring_list ?year ?firstname ?lastname ?tuteur_lastname ?tuteur_firstname (get_mentoring t)

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

  let get_course_name_translation ~label t =
    let label = simplify label in
    let course_entry_opt =
        Course_name_translation.get_course_entry label (get_course_entries t)
    in
    match course_entry_opt with
      | None -> t, (None,None)
      | Some a -> t, (a.Public_data.french_entry,a.Public_data.english_entry)

let add_course_entry unify =
  add_gen
    get_course_entries
    set_course_entries
    (Course_name_translation.add_course_entry unify)

let get_course_entry string  t =
  let course_entry_opt =
    Course_name_translation.get_course_entry string
      (get_course_entries t)
  in
  t, course_entry_opt

let add_course_entry_in_report unify =
  add_gen
    get_course_entries_report
    set_course_entries_report
    (Course_name_translation.add_course_entry unify)

let add_course_entry_in_report unify course t =
  let t =
    {t with Public_data.gps_entry =
              simplify t.Public_data.gps_entry}
  in
  add_course_entry_in_report unify course t

let get_course_entries_report t =
  let map = get_course_entries_report t in
  t,Course_name_translation.to_list map

let add_cursus unify =
  add_gen
    get_cursus
    set_cursus
    (Cursus.add_cursus unify)

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

let add_inscription unify =
      add_gen
        get_inscriptions
        set_inscriptions
        (Inscriptions.add_inscription unify)

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

let add_sorted_course unify =
    add_gen
        get_sorted_courses
        set_sorted_courses
        (Cours_a_trier.add_sorted_course unify)

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

let add_sorted_internship unify =
    add_gen
        get_sorted_internships
        set_sorted_internships
        (Stages_a_trier.add_sorted_internship unify)

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

let add_note_a_modifier unify =
  add_gen
    get_notes_a_modifier
    set_notes_a_modifier
    (Notes_a_modifier.add_note_a_modifier warn unify)

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

let get_dens_candidate
    ~firstname ~lastname ~year t =
  match
    Dens_candidates.get_dens_candidate
              ~firstname ~lastname ~year
              t.data.dens_candidates
  with
  | [] -> t, None
  | [a] -> t, Some a
  | _::_::_ ->
            warn
              __POS__
              "Several dens candidates for the same year and the same student"
              Exit
              t,
            None


let get_minor_candidates
      ~firstname ~lastname ~year t =
      t, Minor_candidates.get_minor_candidate
          ~firstname ~lastname ~year
          t.data.minors

let get_major_candidates
      ~firstname ~lastname ~year t =
      t, Major_candidates.get_major_candidate
        ~firstname ~lastname ~year
        t.data.majors


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

let gen get set =
  let add t elt =
    let elts = get t in
    set (elt::elts) t
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
let add_under_average_validated_grades,get_under_average_validated_grades =
  gen get_under_average_validated_grades set_under_average_validated_grades
let add_out_of_schooling_years,get_out_of_schooling_years =
    gen get_out_of_schooling_years set_out_of_schooling_years
let add_minor_suggestion, get_minor_suggestion_list =
    gen get_minor_suggestions set_minor_suggestions
let add_major_suggestion, get_major_suggestion_list =
    gen get_major_suggestions set_major_suggestions
let add_dens_candidate_suggestion, get_dens_candidates_suggestion_list =
    gen get_dens_candidates_suggestion set_dens_candidates_suggestion

let add_course_to_be_sorted, get_courses_to_be_sorted =
    gen get_courses_to_be_sorted set_courses_to_be_sorted

let add_internship_to_be_sorted, get_internships_to_be_sorted =
        gen get_internships_to_be_sorted set_internships_to_be_sorted

let add_missing_internship_translation, get_missing_internship_translation_list =
    gen get_missing_internship_translations set_missing_internship_translations

let add_dens, get_dens =
  gen get_dens set_dens
let add_national_diploma, get_national_diplomas =
  gen get_national_diplomas set_national_diplomas
let add_mentor, get_mentors =
  gen get_mentors set_mentors
let add_mentor t elt =
  add_mentor t elt

let add_missing_course_entry, get_missing_course_entries =
  gen get_missing_course_entries set_missing_course_entries
let add_missing_mentor, get_missing_mentors =
  gen get_missing_mentors set_missing_mentors
let add_missing_ects_attribution, get_missing_ects_attributions =
  gen get_missing_ects_attributions set_missing_ects_attributions
let add_courses_validated_twice, get_courses_validated_twice =
  gen get_courses_validated_twice set_courses_validated_twice
let add_missing_internship_description, get_missing_internship_descriptions =
  gen get_missing_internship_descriptions set_missing_internship_descriptions
let add_non_validated_internship, get_non_validated_internships =
    gen get_non_validated_internships set_non_validated_internships
let add_ambiguous_internship_description, get_ambiguous_internship_descriptions =
    gen get_ambiguous_internship_descriptions set_ambiguous_internship_descriptions

let get_ENSPSL_logo t =
  let t, local = get_local_repository t in
  let logo = t.parameters.enspsl_logo in
  if local = ""
  then
    t, logo
  else
    t, Format.sprintf "%s/%s" local logo

let get_promo ~firstname ~lastname t =
  let list = get_students t in
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

let open_array pos ?logger ~with_lines ?size ?color ?bgcolor ?align ~title ?title_english t  =
  let t, title  =
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
  in
  open_array pos ?logger ~with_lines ?size ?color ?bgcolor ?align ~title t


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

let get_dens_candidates_list t = t, t.data.dens_candidates
let get_diplomation_year t = t, t.parameters.diplomation_year
