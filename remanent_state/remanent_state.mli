type t

(** initialization *)
val init: unit -> t

(** error handling *)

(** log an exception *)
val warn:
(string * int * int * int)
-> string -> exn  -> t
-> t


val warn_and_log:
  ?logger:Loggers.t
  -> ?backgroundcolor:Sco_tools.Color.color
  -> ?textcolor:Sco_tools.Color.color
  -> ?lineproportion:float
  -> (string * int * int * int)
  -> string -> exn  -> t
  -> t

(** log an exception and output a default value *)
val warn_dft:
  (string * int * int * int)
  -> string -> exn  -> 'a -> t
  -> t * 'a

(** interrupt execution *)
val stop:
(string * int * int * int)
-> string -> exn  -> t
-> t

val print_errors:
  ?logger:Loggers.t -> string -> t -> t

(** log *)
val fprintf:
  ?logger:Loggers.t -> t -> ('a, Format.formatter, unit) format -> 'a

val fprintf_verbatim:
  ?logger:Loggers.t -> t -> ('a, Format.formatter, unit) format -> 'a

val log:
  ?logger:Loggers.t ->
  ?backgroundcolor:Color.color ->
  ?textcolor:Color.color ->
  ?lineproportion:float -> t -> ('a, Format.formatter, unit) format -> 'a

val log_to_string:
  ?logger:Loggers.t ->
  ?backgroundcolor:Color.color ->
  ?textcolor:Color.color ->
  ?lineproportion:float ->
  ?english:string -> t -> string -> string

val log_string:
  ?logger:Loggers.t ->
  ?backgroundcolor:Color.color ->
  ?textcolor:Color.color ->
  ?lineproportion:float ->
  ?english:string -> t -> string -> unit

val open_array:
  (string * int * int * int) ->
  ?colortitle:string -> ?makecell:bool -> ?logger:Loggers.t ->
  with_lines:bool -> ?size:float option list -> ?color: Color.color option list -> ?bgcolor:Color.color option list -> ?align:char option list -> title:string list list -> ?title_english: string list list -> t -> t
val close_array: ?logger:Loggers.t -> t -> unit
val open_row: ?logger:Loggers.t -> ?macro:string -> t -> unit
val close_row: ?logger:Loggers.t -> t -> unit
val print_cell: ?logger:Loggers.t -> ?english:string -> string -> t  -> unit
val print_optional_cell: ?logger:Loggers.t -> ?english:string -> string -> t  -> unit

val breakpage: ?logger:Loggers.t -> t -> unit
val flush:
  ?logger:Loggers.t -> t -> unit

val print_newline:
  ?logger:Loggers.t -> t -> unit

(** profiling *)
val open_event_opt:
  Profiling.step_kind option -> t -> t

val close_event_opt:
  Profiling.step_kind option -> t -> t

(** Interaction with Cloud client *)
val get_cloud_synchronization_mode: t -> t * Public_data.cloud_synchronization_mode
val get_cloud_client: t -> t * Public_data.cloud_client
val get_cloud_repository: t -> t * string
val get_local_repository: t -> t * string
val get_distant_repository: t -> t * string
val get_cloud_client_options: t -> t * string
val get_cloud_support_dynamic_link: t -> t * bool
val get_pdfgenerator_engine: t -> t * Public_data.pdf_generator
val get_pdfgenerator_options: t -> t * string
val get_output_repository: t -> t * string
val get_output_alias_repository: t -> t * string
val get_output_alias: t -> t * (string * string) option
val set_output_alias: t -> (string * string) -> t

(** http access *)
val get_file_retriever:
  t -> t * Public_data.file_retriever
val get_file_retriever_options: ?more_options:(t -> t * string) -> t -> t * string

val get_annuaire_access_options: t -> t * string
val get_gps_access_options: t -> t * string
val get_file_retriever_log_repository: t -> t * string
val get_file_retriever_log_file: t -> t * string
val get_file_retriever_annuaire_tmp_repository: t -> t * string
val get_file_retriever_annuaire_html_file: t -> t * string
val get_file_retriever_time_out_in_second: t -> t * int option
val get_file_retriever_checking_period: t -> t * int

(** gps crawler *)
val get_machine_to_access_gps: t -> t * string
val get_port_to_access_gps: t -> t * string
val get_url_to_access_annuaire: t -> t * string
val get_repository_to_access_gps: t -> t * string
val get_repository_to_dump_gps_files:
  ?output_repository:string -> t -> t * string
val get_repository_for_handmade_gps_files: t -> t * string
val get_repository_for_backup_gps_files: t -> t * string
val get_signature: t -> t * string list
val get_store_output_according_to_their_promotions: t -> t * bool
val get_indicate_promotions_in_gps_file_names: t -> t * bool
val get_repository_to_dump_attestations: t -> t * string
val get_indicate_promotions_in_attestation_file_names: t -> t * bool

val get_url_prefix_for_photos: t -> t * string
val get_rel_url_prefix_for_photos: t -> t * string
val get_correct_rel_url_prefix_for_photos: t -> t * string
val get_include_pictures: t -> t * bool

val get_course_entry_list_repository: t -> t * string

(** CSV *)

val get_csv_separator: t -> t * char option

(** list of students *)

module type Collector =
  sig
    type entry
    type collector
    val get_repository: t -> t * string
    val get: t -> t * collector
    val add: t -> entry -> t
  end

type 'a unification = (string * int * int * int) -> t -> 'a -> 'a -> t * 'a


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
      include Collector_with_unification
      val find_opt: firstname:string -> lastname:string -> year:string ->
        t -> t * entry option
      val find_list: firstname:string -> lastname:string -> year:string ->
          t -> t * entry list
    end

module type Collector_with_search_by_students_wo_year =
    sig
      include Collector_with_unification
      val find_opt: firstname:string ->     lastname:string  ->
        t -> t * entry option
      val find_list: firstname:string ->     lastname:string ->
              t -> t * entry list
    end

    module type Translations =
        sig
          module Collector:Collector_with_unification
          module Report:Collector_with_unification
          module Missing: Collector with type collector = Collector.entry list
          val get_translation: Collector.entry unification -> (string * int * int * int) -> string -> t -> t * (string option * string option)
          val get_report: t -> t * Collector.entry list
        end

(* Warnings about the pictures that are missing *)
module Missing_pictures: Collector
      with type entry = Public_data.student
      and type collector = Public_data.student list

(* Warnings about failure in gps accesses *)
module Gps_server_faillures: Collector
    with type entry = Public_data.student
    and type collector = Public_data.student list

(* Warning about internships *)
module Ambiguous_internship_descriptions: Collector
        with type entry = Public_data.missing_internship_description
        and type collector = Public_data.missing_internship_description list

(* Warning about internships *)
module Non_validated_internships: Collector
        with type entry = Public_data.missing_internship_description
        and type collector = Public_data.missing_internship_description list

module Internships_to_be_sorted: Collector
        with type  entry = Public_data.stage_a_trier
        and type collector = Public_data.stage_a_trier list

module Missing_internship_translations: Collector
       with type  entry = Public_data.internship
       and type collector = Public_data.internship list

module Missing_internship_descriptions:
        Collector
        with type entry = Public_data.missing_internship_description
        and type collector = Public_data.missing_internship_description list

(** Warnings about grades *)
module Missing_grades:
        Collector
        with type entry = Public_data.missing_grade
        and type collector = Public_data.missing_grade list

module Student_ids:
        Collector
        with type entry = Public_data.student_id
        and type collector = Public_data.student_id list
module Non_accepted_grades:
        Collector
        with type entry = Public_data.missing_grade
        and type collector = Public_data.missing_grade list

module Under_average_validated_grades:
        Collector
        with type entry = Public_data.missing_grade
        and type collector = Public_data.missing_grade list

module Missing_ects_attributions:
        Collector
        with type entry = Public_data.missing_grade
        and type collector = Public_data.missing_grade list

module Courses_validated_twice:
        Collector
        with type entry = Public_data.missing_grade
        and type collector = Public_data.missing_grade list

(** Warnings about courses *)
(*module Missing_course_entries: Collector
        with type entry = Public_data.course_entry
        and type collector = Public_data.course_entry list*)

(** Warning about DENS *)
module Dens_candidate_suggestion: Collector
        with type entry = Public_data.dens_candidate
        and type collector = Public_data.dens_candidate list

module Dens_candidate_missing_minors: Collector
        with type entry = Public_data.mineure_majeure
        and type collector = Public_data.mineure_majeure list

module Dens_candidate_missing_majors: Collector
        with type entry = Public_data.mineure_majeure
        and type collector = Public_data.mineure_majeure list

module Course_to_be_sorted: Collector
        with type entry = Public_data.cours_a_trier
        and type collector = Public_data.cours_a_trier list

  (** Other warnings *)
module Grade_out_of_schooling_years: Collector
        with type entry = Public_data.missing_grade
        and type collector = Public_data.missing_grade list

module Missing_mentors: Collector
        with type entry = Public_data.missing_mentor
        and type collector = Public_data.missing_mentor list

module Collector_mentors: Collector
        with type entry = Public_data.mentor
        and type collector = Public_data.mentor list

module Collector_national_diplomas: Collector
        with type entry = Public_data.diplome_national
        and type collector = Public_data.diplome_national list

module Collector_dens_diplomas: Collector
        with type entry = Public_data.dens
        and type collector = Public_data.dens list

module Collector_dens_candidate: Collector_with_search_by_students with type entry = Public_data.dens_candidate and type collector = Dens_candidates.t

module Collector_minor_candidate: Collector_with_search_by_students with type entry = Public_data.mineure_majeure and type collector = Minor_candidates.t

module Collector_major_candidate: Collector_with_search_by_students with type entry = Public_data.mineure_majeure and type collector = Major_candidates.t

module Collector_administrative_status:
   Collector_with_search_by_students with type entry = Public_data.student_pegasus and type collector = Pegasus_administrative_status.t

module Collector_pedagogical_registrations:
      Collector_with_search_by_students_wo_year with type entry = Public_data.pedagogical_entry_pegasus and type collector = Pegasus_pedagogical_registrations.t

module Collector_course_pegasus:
      Collector_with_unification with type entry = Public_data.course_pegasus and type collector = Pegasus_courses.t

module Collector_stages_tries:
Collector_with_unification with type entry =  Public_data.stage_a_trier
and type collector = Stages_a_trier.t

module Collector_scholarships:
  Collector_with_search_by_students_wo_year
  with type entry = Public_data.scholarship
  and type collector = Scholarships.t

module Collector_course_exceptions:
  Collector_with_unification
  with type entry = Public_data.course_exception
  and type collector = Course_exceptions.t

module Translate_courses: Translations
  with type Collector.entry = Public_data.course_entry
  and type Report.entry  = Public_data.course_entry
  and type Missing.entry = Public_data.course_entry
  (*and type Missing.collector = Public_data.course_entry list*)

val get_course_in_pegasus:
  codehelisa: string ->
  year:Public_data.annee ->
  t -> t * Public_data.course_pegasus option

val get_course_exception:
  codegps:string ->
  year:Public_data.annee ->
  t -> t * Public_data.course_exception option

module Collector_departements:
  Collector_with_unification
  with type entry = Public_data.dpt
  and type collector = Departments.t

val get_dpt:
    acronym:string ->
    t ->
    t * Public_data.dpt option

(** list of mentoring *)
val get_monitoring_list_prefix: t -> t * string
val get_monitoring_list_repository: t -> t * string

val get_programs_list_prefix: t -> t * string
val get_programs_list_repository: t -> t * string

val get_cursus_list_prefix: t -> t * string
val get_cursus_list_repository: t -> t * string

val get_inscriptions_list_prefix: t -> t * string
val get_inscriptions_list_repository: t -> t * string

val get_cursus_exceptions_list_prefix: t -> t * string
val get_cursus_exceptions_list_repository: t -> t * string

val get_compensations_list_prefix: t -> t * string
val get_compensations_list_repository: t -> t * string

val get_decisions_list_prefix: t -> t * string
val get_decisions_list_repository: t -> t * string

val get_admissions_list_prefix: t -> t * string
val get_admissions_list_repository: t -> t * string

val get_dispenses_list_prefix: t -> t * string
val get_dispenses_list_repository: t -> t * string

val get_additional_courses_list_prefix: t -> t * string
val get_additional_courses_list_repository: t -> t * string

val get_modified_grades_list_prefix: t -> t * string
val get_modified_grades_list_repository: t -> t * string

val get_launching_date: t -> t * string
val get_comma_symbol: t -> t * char


val get_language: t -> t * Public_data.language
val get_repartition: t -> t * Public_data.repartition

type save_logger
val save_std_logger: t -> save_logger
val restore_std_logger: t -> save_logger -> t
val set_std_logger: t -> Loggers.t -> t

val std_logger: Loggers.t
val close_logger: ?logger:Loggers.t -> t -> t

val get_cost_members:
  t -> Public_data.cost_member list

type pos = string*int*int*int

type 'a unify = (string * int * int * int -> t -> 'a -> 'a -> t * 'a)
type 'a add = 'a unify -> pos -> 'a -> t -> t

val add_cost_member: Public_data.cost_member add

val get_birth_city_fr:
    firstname:string ->
    lastname:string ->
    year:string -> t -> t * string option

val get_birth_country_fr:
        firstname:string ->
        lastname:string ->
        year:string -> t -> t * string option

val get_ine_number:
        firstname:string ->
        lastname:string ->
        year:string -> t -> t * string option

val get_sorted_courses_list_repository: t -> t * string
val add_sorted_course:      (string * int * int * int ->
             t ->
             Public_data.cours_a_trier ->
             Public_data.cours_a_trier -> t * Public_data.cours_a_trier) ->
            string * int * int * int -> Public_data.cours_a_trier -> t -> t

val get_sorted_courses:
   ?firstname:string ->
   ?lastname:string ->
   ?year:string ->
   ?libelle:string ->
   ?codegps:string -> t -> t * Public_data.cours_a_trier list

val get_cost_members_repository: t -> t * string

val get_sorted_internships:
      ?firstname:string ->
      ?lastname:string ->
      ?year:string ->
      ?libelle:string ->
     t -> t * Public_data.stage_a_trier list

(** scholarships *)
val get_scholarship:
  firstname:string ->
  lastname:string ->
  current_year:string ->
  t ->
  t * Public_data.scholarship option

val add_scholarship:
  (string * int * int * int ->
   t ->
   Public_data.scholarship ->
   Public_data.scholarship -> t * Public_data.scholarship) ->
  (string * int * int * int) ->
  Public_data.scholarship ->
  t ->
  t

val add_mentoring:
  (string * int * int * int ->
   t ->
   Public_data.tutorat ->
   Public_data.tutorat -> t * Public_data.tutorat) ->
  (string * int * int * int) ->
  Public_data.tutorat ->
  t ->
  t

val get_mentoring:
  firstname:string ->
  lastname:string ->
  year:Public_data.annee ->
  ?tuteur_gps:Public_data.tutorat ->
  t ->
  t * Public_data.tutorat option

val get_mentoring_list:
  ?firstname:string ->
  ?lastname:string ->
  ?tuteur_firstname:string ->
  ?tuteur_lastname:string ->
  ?year:Public_data.annee ->
  t ->
  t * Public_data.tutorat list

val add_program:
  (string * int * int * int ->
   t ->
   Public_data.program ->
   Public_data.program -> t * Public_data.program) ->
  (string * int * int * int) ->
  Public_data.program ->
  t -> t

val get_program:
  code_gps:string ->
  t -> t * Public_data.program option

val add_cursus:
  (string * int * int * int ->
   t ->
   Public_data.cursus ->
   Public_data.cursus -> t * Public_data.cursus) ->
    (string * int * int * int) ->
    Public_data.cursus ->
    t -> t

val get_cursus:
  year:string ->
  level:string ->
  ?dpt:Public_data.main_dpt  ->
  gpscodelist:string list ->
  ?firstname:string ->
  ?lastname:string ->
  (string * int * int * int) ->
  t -> t * Public_data.cursus option

  val add_inscription:
    (string * int * int * int ->
     t ->
     Public_data.inscription ->
     Public_data.inscription -> t * Public_data.inscription) ->
      (string * int * int * int) ->
      Public_data.inscription ->
      t -> t

  val get_inscription:
    year:string ->
    level:string ->
    ?dpt:Public_data.main_dpt  ->
    lastname:string  ->
    firstname: string ->
    t -> t * Public_data.inscription option


val list_all_cursus:
  t -> unit

val add_cursus_exception:
  (string * int * int * int ->
   t ->
   Public_data.cursus_exception ->
   Public_data.cursus_exception -> t * Public_data.cursus_exception) ->
  (string * int * int * int) ->
  Public_data.cursus_exception ->
  t -> t

val get_cursus_exception:
  firstname:string -> lastname:string ->
  year: string ->
  code_gps:string ->
  t -> t * Public_data.cursus_exception option

val add_decision:
  (string * int * int * int ->
   t ->
   Public_data.decision ->
   Public_data.decision -> t * Public_data.decision) ->
  (string * int * int * int) ->
  Public_data.decision ->
  t -> t

val get_decision:
  firstname:string -> lastname:string -> year:string ->
  program:string -> dpt:Public_data.main_dpt ->
  t -> t * Public_data.decision option

val get_decision_list:
  firstname:string -> lastname:string ->
  ?year:string -> ?program:string -> ?dpt:Public_data.main_dpt ->
    t -> t * Public_data.decision list

val add_admission:
  (string * int * int * int ->
   t ->
   Public_data.admission ->
   Public_data.admission -> t * Public_data.admission) ->
    (string * int * int * int) ->
    Public_data.admission ->
    t -> t

val get_admission:
    firstname:string -> lastname:string -> year:string ->
    t -> t * Public_data.admission option

val add_compensation:
  (string * int * int * int ->
   t ->
   Public_data.compensation ->
   Public_data.compensation -> t * Public_data.compensation) ->
  (string * int * int * int) ->
  Public_data.compensation ->
  t -> t

val get_compensation:
  firstname:string -> lastname:string -> year:string -> codecours:string
  -> t -> t * Public_data.compensation option

val add_dispense:
  (string * int * int * int ->
   t ->
   Public_data.dispense ->
   Public_data.dispense -> t * Public_data.dispense) ->
  (string * int * int * int) ->
  Public_data.dispense ->
  t -> t

val get_dispenses:
  ?firstname:string -> ?lastname:string -> ?year:string -> ?program:string -> ?dpt:string->
  t -> t * Public_data.dispense list

  val add_additional_course:
    (string * int * int * int ->
     t ->
     Public_data.cours_a_ajouter ->
     Public_data.cours_a_ajouter -> t * Public_data.cours_a_ajouter) ->
    (string * int * int * int) ->
    Public_data.cours_a_ajouter ->
    t -> t

  val get_additional_course:
    firstname:string -> lastname:string ->
    t -> t * Public_data.cours_a_ajouter list

val add_note_a_modifier:
  (string * int * int * int ->
   t ->
   Public_data.note_a_modifier ->
     Public_data.note_a_modifier -> t * Public_data.note_a_modifier) ->
    (string * int * int * int) ->
    Public_data.note_a_modifier ->
  t -> t

val get_note_a_modifier:
  firstname:string -> lastname:string -> code:string -> year:string ->
  t -> t * string option

val get_ects_a_modifier:
  firstname:string -> lastname:string -> code:string -> year:string ->
  t -> t * float option

val get_current_academic_year:
  t -> t * Public_data.annee

val get_picture_potential_locations:
  firstname:string ->
  lastname:string ->
  year:Public_data.annee -> t -> t * string list

  val get_picture_write_potential_locations:
    firstname:string ->
    lastname:string ->
    year:Public_data.annee -> t -> t * string list

val get_target:
  t -> t * string option

val get_repository_to_dump_dens_supplement:
  ?output_repository:string -> t -> t * string

val get_ENSPSL_logo: t -> t * string list
val get_ENSPSL_logo_bis: t -> t * string list

val get_commission: t -> t * (string * Public_data.annee) option
val get_main_commission_rep: t -> t * string

val log_mkdir: t -> t * bool

val store_errors_and_profiling_info :
  (string * int * int * int -> t -> string -> t * string) ->
  (string * int * int * int -> t -> string -> string -> t) -> t -> t

val get_promo:
  firstname:string -> lastname:string -> t -> t*string option

val get_student_personnal_repository:
  ?promo:string -> firstname:string -> lastname:string -> t -> t * string

val get_promo_personnal_repository:
  promo:string -> t -> t * string

val get_promos_personnal_repository:
   t -> t * string

val get_students_personnal_files:
  ?language:Public_data.language -> promo:string  -> t -> t * string

val get_main_dpt: t -> t * Public_data.main_dpt
val is_main_dpt_di: t -> t * bool
val is_main_dpt_dma: t -> t * bool
val is_main_dpt_phys: t -> t * bool
val is_main_dpt_chimie: t -> t * bool
val is_main_dpt_geosciences: t -> t * bool

val get_file_retriever_skip: t -> t * bool
val file_retriever_fail: t -> t

val get_commission_rep_from_key: ?commission_rep:string -> ?univ:Public_data.universite -> string -> t -> t * (string * string * string)

val get_commission_rep: ?commission_rep:string -> sous_commission:Public_data.sous_commission -> ?univ:Public_data.universite -> t -> t * (string * string * string)

val get_diplomation_rep: ?firstname:string  -> ?lastname:string ->  t -> t * string option

val push_copy: input_rep:string -> output_rep:string -> file_name:string -> t -> t
val pop_copy:
  copy:(input_rep:string ->
        file_name:string -> output_rep:string -> t -> t)
  -> t -> t
val empty_copy:
  copy:(input_rep:string ->
        file_name:string -> output_rep:string -> t -> t)
  -> t -> t

val get_diplomation_year: t -> t * string option
val get_is_bilingual: t -> t * bool
val bilingual_string: ?english:string -> french:string -> t -> t * string
